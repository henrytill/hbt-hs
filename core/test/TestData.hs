module TestData
  ( TestCase (..)
  , discoverInput
  , discoverOutput
  , testParser
  , testFormatter
  , parserTests
  , formatterTests
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.List.Split qualified as Split
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Data.Yaml qualified as Yaml
import Hbt (Flow (..), Format (..), formatWith, parseWith)
import System.Directory (listDirectory)
import System.FilePath (splitExtensions, (</>))
import Test.Dwergaz
import TestUtilities (testIO)

data TestCase (f :: Flow) = MkTestCase
  { stem :: String
  , format :: Format f
  , input :: Text
  , expected :: Text
  }
  deriving stock (Eq, Ord, Show)

type TestMap f = Map String (TestCase f)

baseDir :: FilePath
baseDir = "test" </> "data"

inputDir :: Format From -> Maybe FilePath
inputDir JSON = Just (baseDir </> "pinboard" </> "json")
inputDir XML = Just (baseDir </> "pinboard" </> "xml")
inputDir Markdown = Just (baseDir </> "markdown")
inputDir HTML = Just (baseDir </> "html")

outputDir :: Format To -> Maybe FilePath
outputDir HTML = Just (baseDir </> "html")
outputDir YAML = Nothing

formatExt :: Format f -> String
formatExt JSON = "json"
formatExt XML = "xml"
formatExt Markdown = "md"
formatExt HTML = "html"
formatExt YAML = "yaml"

readText :: FilePath -> IO Text
readText path = do
  bytes <- BS.readFile path
  pure (Text.Encoding.decodeUtf8With Text.Error.lenientDecode bytes)

split :: FilePath -> (String, String)
split path = fmap (drop 1) (splitExtensions path)

splitExt :: String -> [String]
splitExt s = filter (not . null) (Split.splitOn "." s)

processFile :: Format From -> FilePath -> TestMap From -> FilePath -> IO (TestMap From)
processFile format dir acc file = do
  let (stem, ext) = split file
      fullPath = dir </> file
  case splitExt ext of
    ["expected", "yaml"] -> do
      expected <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {stem, format, input = Text.empty, expected})
            Just tc -> Just (tc {expected})
      pure (Map.alter updater stem acc)
    ["input", e] | e == formatExt format -> do
      input <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {stem, format, input, expected = Text.empty})
            Just tc -> Just (tc {input})
      pure (Map.alter updater stem acc)
    _ -> pure acc

processOutputFile :: Format To -> FilePath -> TestMap To -> FilePath -> IO (TestMap To)
processOutputFile format dir acc file = do
  let (stem, ext) = split file
      fullPath = dir </> file
  case splitExt ext of
    ["expected", e] | e == formatExt format -> do
      expected <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {stem, format, input = Text.empty, expected})
            Just tc -> Just (tc {expected})
      pure (Map.alter updater stem acc)
    ["input", _] -> do
      input <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {stem, format, input, expected = Text.empty})
            Just tc -> Just (tc {input})
      pure (Map.alter updater stem acc)
    _ -> pure acc

discoverInput :: Format From -> IO [TestCase From]
discoverInput format =
  case inputDir format of
    Nothing -> pure []
    Just dir -> do
      allFiles <- listDirectory dir
      testMap <- foldM (processFile format dir) Map.empty allFiles
      let tests = map snd (Map.toList testMap)
      pure (sort tests)

discoverOutput :: Format To -> IO [TestCase To]
discoverOutput format =
  case outputDir format of
    Nothing -> pure []
    Just dir -> do
      allFiles <- listDirectory dir
      testMap <- foldM (processOutputFile format dir) Map.empty allFiles
      let tests = map snd (Map.toList testMap)
      pure (sort tests)

testParser :: TestCase From -> IO Test
testParser testCase = testIO testCase.stem $ do
  expected <- Yaml.decodeThrow (Text.Encoding.encodeUtf8 testCase.expected)
  actual <- parseWith testCase.format testCase.input
  pure (assertEqual testCase.stem expected actual)

testFormatter :: Format From -> TestCase To -> IO Test
testFormatter inputFormat testCase = testIO testCase.stem $ do
  parsed <- parseWith inputFormat testCase.input
  formatted <- formatWith testCase.format parsed
  actualReparsed <- parseWith inputFormat formatted
  expectedReparsed <- parseWith inputFormat testCase.expected
  pure (assertEqual testCase.stem expectedReparsed actualReparsed)

parserTests :: String -> [TestCase From] -> IO Test
parserTests groupName testCases = do
  tests <- traverse testParser testCases
  pure (group groupName tests)

formatterTests :: String -> Format From -> [TestCase To] -> IO Test
formatterTests groupName inputFormat testCases = do
  tests <- traverse (testFormatter inputFormat) testCases
  pure (group groupName tests)
