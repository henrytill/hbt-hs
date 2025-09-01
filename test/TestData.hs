module TestData where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, partition, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Microstache (Template, compileMustacheFile)

testDataBaseDir :: String
testDataBaseDir = "test/data"

htmlTestDataDir :: String
htmlTestDataDir = testDataBaseDir </> "html"

markdownTestDataDir :: String
markdownTestDataDir = testDataBaseDir </> "markdown"

pinboardTestDataDir :: String
pinboardTestDataDir = testDataBaseDir </> "pinboard"

readTextFile :: FilePath -> IO Text
readTextFile path = do
  bytes <- BS.readFile path
  pure (Text.Encoding.decodeUtf8With Text.Error.lenientDecode bytes)

discoverTestCases :: String -> String -> IO [String]
discoverTestCases dir suffix = do
  allFiles <- listDirectory dir
  let inputFiles = [f | f <- allFiles, suffix `isSuffixOf` f]
      testNames = map (takeBaseName . takeBaseName) inputFiles
  pure (sort testNames)

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

data AllTestData = AllTestData
  { htmlParserTests :: [HtmlTestCase]
  , markdownTests :: [SimpleTestCase]
  , pinboardJsonTests :: [PinboardTestCase]
  , pinboardXmlTests :: [PinboardTestCase]
  , htmlFormatterTests :: [HtmlFormatterTestCase]
  }

discoverHtmlTestCaseNames :: IO [String]
discoverHtmlTestCaseNames = discoverTestCases htmlTestDataDir ".input.html"

loadHtmlTestCase :: String -> IO HtmlTestCase
loadHtmlTestCase testName = do
  let inputFile = htmlTestDataDir </> (testName ++ ".input.html")
      expectedFile = htmlTestDataDir </> (testName ++ ".expected.yaml")
  inputHtml <- readTextFile inputFile
  expectedYaml <- readTextFile expectedFile
  pure HtmlTestCase {testName, inputHtml, expectedYaml}

loadAllHtmlTestCases :: IO [HtmlTestCase]
loadAllHtmlTestCases = do
  testCaseNames <- discoverHtmlTestCaseNames
  mapM loadHtmlTestCase testCaseNames

discoverMarkdownTestCaseNames :: IO [String]
discoverMarkdownTestCaseNames = discoverTestCases markdownTestDataDir ".input.md"

loadSimpleTestCase :: String -> IO SimpleTestCase
loadSimpleTestCase testName = do
  let inputFile = markdownTestDataDir </> (testName ++ ".input.md")
      expectedFile = markdownTestDataDir </> (testName ++ ".expected.yaml")
  inputMarkdown <- readTextFile inputFile
  expectedYaml <- readTextFile expectedFile
  pure SimpleTestCase {testName, inputMarkdown, expectedYaml}

loadAllSimpleTestCases :: IO [SimpleTestCase]
loadAllSimpleTestCases = do
  testCaseNames <- discoverMarkdownTestCaseNames
  mapM loadSimpleTestCase testCaseNames

discoverPinboardTestCaseNames :: IO [(String, String)] -- (name, format)
discoverPinboardTestCaseNames = do
  allFiles <- listDirectory pinboardTestDataDir
  let jsonFiles = [f | f <- allFiles, ".input.json" `isSuffixOf` f]
      xmlFiles = [f | f <- allFiles, ".input.xml" `isSuffixOf` f]
      jsonNames = map (\f -> (takeBaseName (takeBaseName f), "json")) jsonFiles
      xmlNames = map (\f -> (takeBaseName (takeBaseName f), "xml")) xmlFiles
  pure (sort (jsonNames ++ xmlNames))

loadPinboardTestCase :: String -> String -> IO PinboardTestCase
loadPinboardTestCase name format = do
  let testName = name ++ "_" ++ format
      inputFile = pinboardTestDataDir </> (name ++ ".input." ++ format)
      expectedFile = pinboardTestDataDir </> (name ++ ".expected.yaml")
  inputText <- readTextFile inputFile
  expectedYaml <- readTextFile expectedFile
  pure PinboardTestCase {testName, inputText, expectedYaml, format}

loadAllPinboardTestCases :: IO [PinboardTestCase]
loadAllPinboardTestCases = do
  testCaseNames <- discoverPinboardTestCaseNames
  mapM (uncurry loadPinboardTestCase) testCaseNames

discoverHtmlFormatterTestCaseNames :: IO [String]
discoverHtmlFormatterTestCaseNames = discoverTestCases htmlTestDataDir ".input.html"

loadHtmlFormatterTestCase :: String -> IO HtmlFormatterTestCase
loadHtmlFormatterTestCase testName = do
  let inputFile = htmlTestDataDir </> (testName ++ ".input.html")
      expectedFile = htmlTestDataDir </> (testName ++ ".expected.html")
  inputHtml <- readTextFile inputFile
  expectedHtml <- readTextFile expectedFile
  template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
  pure HtmlFormatterTestCase {testName, inputHtml, expectedHtml, template}

loadAllHtmlFormatterTestCases :: IO [HtmlFormatterTestCase]
loadAllHtmlFormatterTestCases = do
  testCaseNames <- discoverHtmlFormatterTestCaseNames
  mapM loadHtmlFormatterTestCase testCaseNames

loadAllTestData :: IO AllTestData
loadAllTestData = do
  allPinboardTests <- loadAllPinboardTestCases
  let isJson tc = tc.format == "json"
      (pinboardJsonTests, pinboardXmlTests) = partition isJson allPinboardTests
  AllTestData
    <$> loadAllHtmlTestCases
    <*> loadAllSimpleTestCases
    <*> pure pinboardJsonTests
    <*> pure pinboardXmlTests
    <*> loadAllHtmlFormatterTestCases
