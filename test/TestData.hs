{-# LANGUAGE GADTs #-}

module TestData
  ( HtmlParserTestCase (..)
  , MarkdownParserTestCase (..)
  , PinboardParserTestCase (..)
  , HtmlFormatterTestCase (..)
  , AllTestData (..)
  , loadAllTestData
  )
where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, partition, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Hbt (Format (..), InputFormat, toString)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Microstache (Template, compileMustacheFile)

baseDir :: FilePath
baseDir = "test" </> "data"

htmlDir :: FilePath
htmlDir = baseDir </> "html"

markdownDir :: FilePath
markdownDir = baseDir </> "markdown"

pinboardDir :: FilePath
pinboardDir = baseDir </> "pinboard"

readText :: FilePath -> IO Text
readText path = do
  bytes <- BS.readFile path
  pure (Text.Encoding.decodeUtf8With Text.Error.lenientDecode bytes)

discover :: FilePath -> String -> IO [String]
discover dir suffix = do
  allFiles <- listDirectory dir
  let inputFiles = [f | f <- allFiles, suffix `isSuffixOf` f]
      testNames = map (takeBaseName . takeBaseName) inputFiles
  pure (sort testNames)

data HtmlParserTestCase = MkHtmlParserTestCase
  { testName :: String
  , inputHtml :: Text
  , expectedYaml :: Text
  }
  deriving (Show, Eq)

data MarkdownParserTestCase = MkMarkdownParserTestCase
  { testName :: String
  , inputMarkdown :: Text
  , expectedYaml :: Text
  }
  deriving (Show, Eq)

data PinboardParserTestCase = MkPinboardParserTestCase
  { testName :: String
  , inputText :: Text
  , expectedYaml :: Text
  , format :: InputFormat
  }
  deriving (Show, Eq)

data HtmlFormatterTestCase = MkHtmlFormatterTestCase
  { testName :: String
  , inputHtml :: Text
  , expectedHtml :: Text
  , template :: Template
  }

data AllTestData = MkAllTestData
  { htmlParserTests :: [HtmlParserTestCase]
  , markdownTests :: [MarkdownParserTestCase]
  , pinboardJsonTests :: [PinboardParserTestCase]
  , pinboardXmlTests :: [PinboardParserTestCase]
  , htmlFormatterTests :: [HtmlFormatterTestCase]
  }

discoverHtml :: IO [String]
discoverHtml = discover htmlDir ".input.html"

loadHtml :: String -> IO HtmlParserTestCase
loadHtml testName = do
  let inputFile = htmlDir </> (testName ++ ".input.html")
      expectedFile = htmlDir </> (testName ++ ".expected.yaml")
  inputHtml <- readText inputFile
  expectedYaml <- readText expectedFile
  pure MkHtmlParserTestCase {testName, inputHtml, expectedYaml}

loadAllHtml :: IO [HtmlParserTestCase]
loadAllHtml = do
  testCaseNames <- discoverHtml
  mapM loadHtml testCaseNames

discoverMarkdown :: IO [String]
discoverMarkdown = discover markdownDir ".input.md"

loadMarkdown :: String -> IO MarkdownParserTestCase
loadMarkdown testName = do
  let inputFile = markdownDir </> (testName ++ ".input.md")
      expectedFile = markdownDir </> (testName ++ ".expected.yaml")
  inputMarkdown <- readText inputFile
  expectedYaml <- readText expectedFile
  pure MkMarkdownParserTestCase {testName, inputMarkdown, expectedYaml}

loadAllMarkdown :: IO [MarkdownParserTestCase]
loadAllMarkdown = do
  testCaseNames <- discoverMarkdown
  mapM loadMarkdown testCaseNames

discoverPinboard :: IO [(String, InputFormat)] -- (name, format)
discoverPinboard = do
  allFiles <- listDirectory pinboardDir
  let jsonFiles = [f | f <- allFiles, ".input.json" `isSuffixOf` f]
      xmlFiles = [f | f <- allFiles, ".input.xml" `isSuffixOf` f]
      jsonNames = map (\f -> (takeBaseName (takeBaseName f), JSON)) jsonFiles
      xmlNames = map (\f -> (takeBaseName (takeBaseName f), XML)) xmlFiles
  pure (sort (jsonNames ++ xmlNames))

loadPinboard :: String -> InputFormat -> IO PinboardParserTestCase
loadPinboard name format = do
  let formatStr = toString format
      testName = name ++ "_" ++ formatStr
      inputFile = pinboardDir </> (name ++ ".input." ++ formatStr)
      expectedFile = pinboardDir </> (name ++ ".expected.yaml")
  inputText <- readText inputFile
  expectedYaml <- readText expectedFile
  pure MkPinboardParserTestCase {testName, inputText, expectedYaml, format}

loadAllPinboard :: IO [PinboardParserTestCase]
loadAllPinboard = do
  testCaseNames <- discoverPinboard
  mapM (uncurry loadPinboard) testCaseNames

discoverHtmlFormatter :: IO [String]
discoverHtmlFormatter = discover htmlDir ".input.html"

loadHtmlFormatter :: String -> IO HtmlFormatterTestCase
loadHtmlFormatter testName = do
  let inputFile = htmlDir </> (testName ++ ".input.html")
      expectedFile = htmlDir </> (testName ++ ".expected.html")
  inputHtml <- readText inputFile
  expectedHtml <- readText expectedFile
  template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
  pure MkHtmlFormatterTestCase {testName, inputHtml, expectedHtml, template}

loadAllHtmlFormatter :: IO [HtmlFormatterTestCase]
loadAllHtmlFormatter = do
  testCaseNames <- discoverHtmlFormatter
  mapM loadHtmlFormatter testCaseNames

loadAllTestData :: IO AllTestData
loadAllTestData = do
  allPinboardTests <- loadAllPinboard
  let isJson tc = tc.format == JSON
      (pinboardJsonTests, pinboardXmlTests) = partition isJson allPinboardTests
  MkAllTestData
    <$> loadAllHtml
    <*> loadAllMarkdown
    <*> pure pinboardJsonTests
    <*> pure pinboardXmlTests
    <*> loadAllHtmlFormatter
