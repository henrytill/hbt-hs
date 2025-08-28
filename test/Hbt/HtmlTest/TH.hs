{-# LANGUAGE TemplateHaskellQuotes #-}

module Hbt.HtmlTest.TH where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

data HtmlTestCase = HtmlTestCase
  { testName :: String
  , inputHtml :: Text
  , expectedYaml :: Text
  }
  deriving (Show, Eq, Lift)

discoverHtmlTestCaseNames :: IO [String]
discoverHtmlTestCaseNames = do
  let testDataDir = "test/data/html"
  allFiles <- listDirectory testDataDir
  let inputFiles = [f | f <- allFiles, ".input.html" `isSuffixOf` f]
  let testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

loadAllHtmlTestDataTH :: Q Exp
loadAllHtmlTestDataTH = do
  testCases <- runIO loadAllHtmlTestCases
  [|testCases|]
  where
    loadAllHtmlTestCases :: IO [HtmlTestCase]
    loadAllHtmlTestCases = do
      testCaseNames <- discoverHtmlTestCaseNames
      mapM loadHtmlTestCase testCaseNames

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
