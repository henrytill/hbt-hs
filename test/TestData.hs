module TestData where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, partition, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Data.Text.IO qualified as Text.IO
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Microstache (Template, compileMustacheFile)

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
discoverHtmlTestCaseNames = do
  let testDataDir = "test/data/html"
  allFiles <- listDirectory testDataDir
  let inputFiles = [f | f <- allFiles, ".input.html" `isSuffixOf` f]
  let testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

loadHtmlTestCase :: String -> IO HtmlTestCase
loadHtmlTestCase name = do
  let testDataDir = "test/data/html"
      inputFile = testDataDir </> (name ++ ".input.html")
      expectedFile = testDataDir </> (name ++ ".expected.yaml")

  -- Read with lenient UTF-8 decoding to handle encoding issues
  inputBytes <- BS.readFile inputFile
  let inputContent = Text.Encoding.decodeUtf8With Text.Error.lenientDecode inputBytes
  expectedBytes <- BS.readFile expectedFile
  let expectedContent = Text.Encoding.decodeUtf8With Text.Error.lenientDecode expectedBytes

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
discoverTestCaseNames :: IO [String]
discoverTestCaseNames = do
  let testDataDir = "test/data/markdown"
  allFiles <- listDirectory testDataDir
  let inputFiles = [f | f <- allFiles, ".input.md" `isSuffixOf` f]
  let testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

loadSimpleTestCase :: String -> IO SimpleTestCase
loadSimpleTestCase name = do
  let testDataDir = "test/data/markdown"
      inputFile = testDataDir </> (name ++ ".input.md")
      expectedFile = testDataDir </> (name ++ ".expected.yaml")

  inputContent <- Text.IO.readFile inputFile
  expectedContent <- Text.IO.readFile expectedFile

  return
    SimpleTestCase
      { testName = name
      , inputMarkdown = inputContent
      , expectedYaml = expectedContent
      }

loadAllSimpleTestCases :: IO [SimpleTestCase]
loadAllSimpleTestCases = do
  testCaseNames <- discoverTestCaseNames
  mapM loadSimpleTestCase testCaseNames

-- Pinboard test data loading
discoverPinboardTestCaseNames :: IO [(String, String)] -- (name, format)
discoverPinboardTestCaseNames = do
  let testDataDir = "test/data/pinboard"
  allFiles <- listDirectory testDataDir
  let jsonFiles = [f | f <- allFiles, ".input.json" `isSuffixOf` f]
      xmlFiles = [f | f <- allFiles, ".input.xml" `isSuffixOf` f]
      jsonNames = map (\f -> (takeBaseName . takeBaseName $ f, "json")) jsonFiles
      xmlNames = map (\f -> (takeBaseName . takeBaseName $ f, "xml")) xmlFiles
  return $ sort $ jsonNames ++ xmlNames

loadPinboardTestCase :: String -> String -> IO PinboardTestCase
loadPinboardTestCase name format = do
  let testDataDir = "test/data/pinboard"
      inputFile = testDataDir </> (name ++ ".input." ++ format)
      expectedFile = testDataDir </> (name ++ ".expected.yaml")

  inputBytes <- BS.readFile inputFile
  let inputContent = Text.Encoding.decodeUtf8With Text.Error.lenientDecode inputBytes
  expectedBytes <- BS.readFile expectedFile
  let expectedContent = Text.Encoding.decodeUtf8With Text.Error.lenientDecode expectedBytes

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
discoverHtmlFormatterTestCaseNames = do
  let testDataDir = "test/data/html"
  allFiles <- listDirectory testDataDir
  let inputFiles = [f | f <- allFiles, ".input.html" `isSuffixOf` f]
  let testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

loadHtmlFormatterTestCase :: String -> IO HtmlFormatterTestCase
loadHtmlFormatterTestCase name = do
  let testDataDir = "test/data/html"
      inputFile = testDataDir </> (name ++ ".input.html")
      expectedFile = testDataDir </> (name ++ ".expected.html")

  -- Read with lenient UTF-8 decoding to handle encoding issues
  inputBytes <- BS.readFile inputFile
  let inputContent = Text.Encoding.decodeUtf8With Text.Error.lenientDecode inputBytes
  expectedBytes <- BS.readFile expectedFile
  let expectedContent = Text.Encoding.decodeUtf8With Text.Error.lenientDecode expectedBytes

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
