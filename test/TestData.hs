module TestData where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, partition, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Microstache (Template, compileMustacheFile)

-- Test data directory constants
testDataBaseDir :: String
testDataBaseDir = "test/data"

htmlTestDataDir :: String
htmlTestDataDir = testDataBaseDir </> "html"

markdownTestDataDir :: String
markdownTestDataDir = testDataBaseDir </> "markdown"

pinboardTestDataDir :: String
pinboardTestDataDir = testDataBaseDir </> "pinboard"

-- Unified file reading with lenient UTF-8 decoding
readTextFile :: FilePath -> IO Text
readTextFile path = do
  bytes <- BS.readFile path
  return $ Text.Encoding.decodeUtf8With Text.Error.lenientDecode bytes

-- Generic test case discovery
discoverTestCases :: String -> String -> IO [String]
discoverTestCases dir suffix = do
  allFiles <- listDirectory dir
  let inputFiles = [f | f <- allFiles, suffix `isSuffixOf` f]
      testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

-- Test case data types
data HtmlTestCase = HtmlTestCase
  { testName :: String
  , inputHtml :: Text
  , expectedYaml :: Text
  }
  deriving (Show, Eq)

data SimpleTestCase = SimpleTestCase
  { testName :: String
  , inputMarkdown :: Text
  , expectedYaml :: Text
  }
  deriving (Show, Eq)

data PinboardTestCase = PinboardTestCase
  { testName :: String
  , inputText :: Text
  , expectedYaml :: Text
  , format :: String -- "json" or "xml"
  }
  deriving (Show, Eq)

data HtmlFormatterTestCase = HtmlFormatterTestCase
  { testName :: String
  , inputHtml :: Text
  , expectedHtml :: Text
  , template :: Template
  }

-- Test data for all test suites
data AllTestData = AllTestData
  { htmlParserTests :: [HtmlTestCase]
  , markdownTests :: [SimpleTestCase]
  , pinboardJsonTests :: [PinboardTestCase]
  , pinboardXmlTests :: [PinboardTestCase]
  , htmlFormatterTests :: [HtmlFormatterTestCase]
  }

-- HTML Parser test data loading
discoverHtmlTestCaseNames :: IO [String]
discoverHtmlTestCaseNames = discoverTestCases htmlTestDataDir ".input.html"

loadHtmlTestCase :: String -> IO HtmlTestCase
loadHtmlTestCase name = do
  let inputFile = htmlTestDataDir </> (name ++ ".input.html")
      expectedFile = htmlTestDataDir </> (name ++ ".expected.yaml")

  inputContent <- readTextFile inputFile
  expectedContent <- readTextFile expectedFile

  return
    HtmlTestCase
      { testName = name
      , inputHtml = inputContent
      , expectedYaml = expectedContent
      }

loadAllHtmlTestCases :: IO [HtmlTestCase]
loadAllHtmlTestCases = do
  testCaseNames <- discoverHtmlTestCaseNames
  mapM loadHtmlTestCase testCaseNames

-- Markdown test data loading
discoverMarkdownTestCaseNames :: IO [String]
discoverMarkdownTestCaseNames = discoverTestCases markdownTestDataDir ".input.md"

loadSimpleTestCase :: String -> IO SimpleTestCase
loadSimpleTestCase name = do
  let inputFile = markdownTestDataDir </> (name ++ ".input.md")
      expectedFile = markdownTestDataDir </> (name ++ ".expected.yaml")

  inputContent <- readTextFile inputFile
  expectedContent <- readTextFile expectedFile

  return
    SimpleTestCase
      { testName = name
      , inputMarkdown = inputContent
      , expectedYaml = expectedContent
      }

loadAllSimpleTestCases :: IO [SimpleTestCase]
loadAllSimpleTestCases = do
  testCaseNames <- discoverMarkdownTestCaseNames
  mapM loadSimpleTestCase testCaseNames

-- Pinboard test data loading
discoverPinboardTestCaseNames :: IO [(String, String)] -- (name, format)
discoverPinboardTestCaseNames = do
  allFiles <- listDirectory pinboardTestDataDir
  let jsonFiles = [f | f <- allFiles, ".input.json" `isSuffixOf` f]
      xmlFiles = [f | f <- allFiles, ".input.xml" `isSuffixOf` f]
      jsonNames = map (\f -> (takeBaseName . takeBaseName $ f, "json")) jsonFiles
      xmlNames = map (\f -> (takeBaseName . takeBaseName $ f, "xml")) xmlFiles
  return $ sort $ jsonNames ++ xmlNames

loadPinboardTestCase :: String -> String -> IO PinboardTestCase
loadPinboardTestCase name format = do
  let inputFile = pinboardTestDataDir </> (name ++ ".input." ++ format)
      expectedFile = pinboardTestDataDir </> (name ++ ".expected.yaml")

  inputContent <- readTextFile inputFile
  expectedContent <- readTextFile expectedFile

  return
    PinboardTestCase
      { testName = name ++ "_" ++ format
      , inputText = inputContent
      , expectedYaml = expectedContent
      , format = format
      }

loadAllPinboardTestCases :: IO [PinboardTestCase]
loadAllPinboardTestCases = do
  testCaseNames <- discoverPinboardTestCaseNames
  mapM (uncurry loadPinboardTestCase) testCaseNames

-- HTML Formatter test data loading
discoverHtmlFormatterTestCaseNames :: IO [String]
discoverHtmlFormatterTestCaseNames = discoverTestCases htmlTestDataDir ".input.html"

loadHtmlFormatterTestCase :: String -> IO HtmlFormatterTestCase
loadHtmlFormatterTestCase name = do
  let inputFile = htmlTestDataDir </> (name ++ ".input.html")
      expectedFile = htmlTestDataDir </> (name ++ ".expected.html")

  inputContent <- readTextFile inputFile
  expectedContent <- readTextFile expectedFile

  -- Load the template
  template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"

  return
    HtmlFormatterTestCase
      { testName = name
      , inputHtml = inputContent
      , expectedHtml = expectedContent
      , template = template
      }

loadAllHtmlFormatterTestCases :: IO [HtmlFormatterTestCase]
loadAllHtmlFormatterTestCases = do
  testCaseNames <- discoverHtmlFormatterTestCaseNames
  mapM loadHtmlFormatterTestCase testCaseNames

-- Load all test data
loadAllTestData :: IO AllTestData
loadAllTestData = do
  htmlParserTests <- loadAllHtmlTestCases
  markdownTests <- loadAllSimpleTestCases
  allPinboardTests <- loadAllPinboardTestCases
  htmlFormatterTests <- loadAllHtmlFormatterTestCases

  let isJson tc = tc.format == "json"
      (pinboardJsonTests, pinboardXmlTests) = partition isJson allPinboardTests

  return
    AllTestData
      { htmlParserTests
      , markdownTests
      , pinboardJsonTests
      , pinboardXmlTests
      , htmlFormatterTests
      }
