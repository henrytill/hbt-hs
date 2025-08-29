{-# LANGUAGE TemplateHaskellQuotes #-}

module Hbt.Formatter.HTMLTest.TH where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

data HtmlFormatterTestCase = HtmlFormatterTestCase
  { testName :: String
  , inputHtml :: Text
  , expectedHtml :: Text
  }
  deriving (Show, Eq, Lift)

discoverHtmlFormatterTestCaseNames :: IO [String]
discoverHtmlFormatterTestCaseNames = do
  let testDataDir = "test/data/html"
  allFiles <- listDirectory testDataDir
  let inputFiles = [f | f <- allFiles, ".input.html" `isSuffixOf` f]
  let testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

loadAllHtmlFormatterTestDataTH :: Q Exp
loadAllHtmlFormatterTestDataTH = do
  testCases <- runIO loadAllHtmlFormatterTestCases
  [|testCases|]
  where
    loadAllHtmlFormatterTestCases :: IO [HtmlFormatterTestCase]
    loadAllHtmlFormatterTestCases = do
      testCaseNames <- discoverHtmlFormatterTestCaseNames
      mapM loadHtmlFormatterTestCase testCaseNames

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

      return
        HtmlFormatterTestCase
          { testName = name
          , inputHtml = inputContent
          , expectedHtml = expectedContent
          }
