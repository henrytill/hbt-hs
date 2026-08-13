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
import Control.Monad qualified as Monad
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Data.Yaml qualified as Yaml
import Hbt (Flow (..), Format (..), SFlow (..), formatWith, parseWith)
import Hbt.Collection (CollectionRepr)
import Hbt.Collection qualified as Collection
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

baseDir :: FilePath
baseDir = "test" </> "data"

-- | Where a category's fixtures live. A category is named by the format its
-- inputs are written in; both the parser and the formatter suites read the
-- same directory, differing only in which expected file they compare against.
categoryDir :: Format From -> FilePath
categoryDir JSON = baseDir </> "pinboard" </> "json"
categoryDir XML = baseDir </> "pinboard" </> "xml"
categoryDir Markdown = baseDir </> "markdown"
categoryDir HTML = baseDir </> "html"

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

-- | A case being assembled: either half may still be missing.
data Partial = MkPartial
  { partialInput :: Maybe Text
  , partialExpected :: Maybe Text
  }

emptyPartial :: Partial
emptyPartial = MkPartial {partialInput = Nothing, partialExpected = Nothing}

processFile :: Format From -> String -> FilePath -> PartialMap -> FilePath -> IO PartialMap
processFile inputFormat expectedExt dir acc file =
  case parts of
    ["input", e] | e == formatExt inputFormat -> updateWith (\p t -> p {partialInput = Just t})
    ["expected", e] | e == expectedExt -> updateWith (\p t -> p {partialExpected = Just t})
    _ -> pure acc
  where
    (stem, ext) = split file
    fullPath = dir </> file
    parts = splitExt ext
    updateWith field = do
      text <- readText fullPath
      pure (Map.alter (Just . flip field text . Maybe.fromMaybe emptyPartial) stem acc)

-- | Assemble the cases in a category, refusing to hand back a suite that would
-- pass without testing anything.
--
-- An uninitialized submodule leaves the directories missing or empty, and an
-- empty list of cases is a suite of no tests: it passes, and the entire golden
-- set is silently gone. A fixture with only one of its two halves is the same
-- failure in miniature - it would have been compared against the empty string.
discover :: Format From -> String -> Format f -> IO [TestCase f]
discover inputFormat expectedExt format = do
  allFiles <- listDirectory dir
  partials <- foldM (processFile inputFormat expectedExt dir) Map.empty allFiles
  cases <- traverse complete (Map.toList partials)
  Monad.when (List.null cases) $
    fail $
      "no test cases found in "
        ++ dir
        ++ " (looking for *.input."
        ++ formatExt inputFormat
        ++ " alongside *.expected."
        ++ expectedExt
        ++ "); is the test/data submodule initialized?"
  pure (sort cases)
  where
    dir = categoryDir inputFormat

    complete (stem, partial) =
      case (partial.partialInput, partial.partialExpected) of
        (Just input, Just expected) -> pure (MkTestCase {stem, format, input, expected})
        (Nothing, _) -> missing stem ("input." ++ formatExt inputFormat)
        (_, Nothing) -> missing stem ("expected." ++ expectedExt)

    missing stem what =
      fail (dir </> stem ++ " has no " ++ stem ++ "." ++ what)

type PartialMap = Map String Partial

discoverInput :: Format From -> IO [TestCase From]
discoverInput inputFormat = discover inputFormat "yaml" inputFormat

discoverOutput :: Format From -> Format To -> IO [TestCase To]
discoverOutput inputFormat outputFormat =
  discover inputFormat (formatExt outputFormat) outputFormat

testParser :: TestCase From -> IO Test
testParser testCase = testIO testCase.stem $ do
  repr <- Yaml.decodeThrow @IO @CollectionRepr (Text.Encoding.encodeUtf8 testCase.expected)
  expected <- Collection.fromRepr repr
  actual <- parseWith testCase.format testCase.input
  pure (assertEqual testCase.stem expected actual)

-- | Compare formatter output against the fixture.
--
-- YAML is compared as a parsed document rather than byte by byte. Emitters
-- disagree about when a scalar needs quoting and where to fold a long line -
-- the fixtures quote a URL containing '#', this one does not - and the two
-- spellings mean the same thing. Comparing documents stays sensitive to what
-- does matter: a field present on one side and absent on the other, or holding
-- a different value.
--
-- HTML has no such parser to hand, so it is compared by reparsing both sides
-- into collections. That is weaker - it cannot see a difference that survives
-- a round trip - which is why the escaping and LAST_MODIFIED defects it missed
-- have unit tests of their own.
testFormatter :: Format From -> TestCase To -> IO Test
testFormatter inputFormat testCase = testIO testCase.stem $ do
  parsed <- parseWith inputFormat testCase.input
  formatted <- formatWith testCase.format parsed
  case testCase.format of
    YAML -> do
      actual <- Yaml.decodeThrow @IO @Yaml.Value (Text.Encoding.encodeUtf8 formatted)
      expected <- Yaml.decodeThrow @IO @Yaml.Value (Text.Encoding.encodeUtf8 testCase.expected)
      pure (assertEqual testCase.stem expected actual)
    HTML -> do
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
