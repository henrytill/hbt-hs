{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeData #-}

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
import Data.List (groupBy, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Data.Yaml qualified as Yaml
import Hbt (Flow (..), Format (..), formatWith, parseWith)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Dwergaz
import TestUtilities (testIO)

data TestCase (f :: Flow) = MkTestCase
  { name :: String
  , format :: Format f
  , input :: Text
  , expected :: Text
  }
  deriving (Show)

instance Eq (TestCase f) where
  a == b = a.name == b.name

instance Ord (TestCase f) where
  compare a b = compare a.name b.name

type TestMap f = Map FilePath (TestCase f)

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

splitExt :: String -> [String]
splitExt s = filter (/= ".") (groupBy (\a b -> a /= '.' && b /= '.') s)

processFile :: Format From -> FilePath -> TestMap From -> FilePath -> IO (TestMap From)
processFile format dir acc file = do
  let name = takeBaseName (takeBaseName file)
      ext = drop 1 (snd (span (/= '.') file))
      fullPath = dir </> file
  case splitExt ext of
    ["expected", "yaml"] -> do
      expected <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {name, format, input = Text.empty, expected})
            Just tc -> Just (tc {expected})
      pure (Map.alter updater name acc)
    ["input", e] | e == formatExt format -> do
      input <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {name, format, input, expected = Text.empty})
            Just tc -> Just (tc {input})
      pure (Map.alter updater name acc)
    _ -> pure acc

processOutputFile :: Format To -> FilePath -> TestMap To -> FilePath -> IO (TestMap To)
processOutputFile format dir acc file = do
  let name = takeBaseName (takeBaseName file)
      ext = drop 1 (snd (span (/= '.') file))
      fullPath = dir </> file
  case splitExt ext of
    ["expected", e] | e == formatExt format -> do
      expected <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {name, format, input = Text.empty, expected})
            Just tc -> Just (tc {expected})
      pure (Map.alter updater name acc)
    ["input", _] -> do
      input <- readText fullPath
      let updater maybeCase = case maybeCase of
            Nothing -> Just (MkTestCase {name, format, input, expected = Text.empty})
            Just tc -> Just (tc {input})
      pure (Map.alter updater name acc)
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
testParser testCase = testIO testCase.name $ do
  expected <- Yaml.decodeThrow (Text.Encoding.encodeUtf8 testCase.expected)
  actual <- parseWith testCase.format testCase.input
  pure (assertEqual testCase.name expected actual)

testFormatter :: Format From -> TestCase To -> IO Test
testFormatter inputFormat testCase = testIO testCase.name $ do
  parsed <- parseWith inputFormat testCase.input
  formatted <- formatWith testCase.format parsed
  actualReparsed <- parseWith inputFormat formatted
  expectedReparsed <- parseWith inputFormat testCase.expected
  pure (assertEqual testCase.name expectedReparsed actualReparsed)

parserTests :: String -> [TestCase From] -> IO Test
parserTests groupName testCases = do
  tests <- traverse testParser testCases
  pure (group groupName tests)

formatterTests :: String -> Format From -> [TestCase To] -> IO Test
formatterTests groupName inputFormat testCases = do
  tests <- traverse (testFormatter inputFormat) testCases
  pure (group groupName tests)
