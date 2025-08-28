{-# LANGUAGE TemplateHaskellQuotes #-}

module Hbt.PinboardTest.TH where

import Data.ByteString qualified as BS
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Error
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

data PinboardTestCase = PinboardTestCase
  { testName :: String
  , inputText :: Text
  , expectedYaml :: Text
  , format :: String -- "json" or "xml"
  }
  deriving (Show, Eq, Lift)

discoverPinboardTestCaseNames :: IO [(String, String)] -- (name, format)
discoverPinboardTestCaseNames = do
  let testDataDir = "test/data/pinboard"
  allFiles <- listDirectory testDataDir
  let jsonFiles = [f | f <- allFiles, ".input.json" `isSuffixOf` f]
      xmlFiles = [f | f <- allFiles, ".input.xml" `isSuffixOf` f]
      jsonNames = map (\f -> (takeBaseName . takeBaseName $ f, "json")) jsonFiles
      xmlNames = map (\f -> (takeBaseName . takeBaseName $ f, "xml")) xmlFiles
  return $ sort $ jsonNames ++ xmlNames

loadAllPinboardTestDataTH :: Q Exp
loadAllPinboardTestDataTH = do
  testCases <- runIO loadAllPinboardTestCases
  [|testCases|]
  where
    loadAllPinboardTestCases :: IO [PinboardTestCase]
    loadAllPinboardTestCases = do
      testCaseNames <- discoverPinboardTestCaseNames
      mapM (uncurry loadPinboardTestCase) testCaseNames

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
