{-# LANGUAGE TemplateHaskell #-}

module Hbt.MarkdownTest.TH where

import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

data SimpleTestCase = SimpleTestCase
  { testName :: String
  , inputMarkdown :: Text
  , expectedYaml :: Text
  }
  deriving (Show, Eq, Lift)

discoverTestCaseNames :: IO [String]
discoverTestCaseNames = do
  let testDataDir = "test/data/markdown"
  allFiles <- listDirectory testDataDir
  let inputFiles = [f | f <- allFiles, ".input.md" `isSuffixOf` f]
  let testNames = map (takeBaseName . takeBaseName) inputFiles
  return $ sort testNames

loadAllTestDataTH :: Q Exp
loadAllTestDataTH = do
  testCases <- runIO loadAllSimpleTestCases
  [|testCases|]
  where
    loadAllSimpleTestCases :: IO [SimpleTestCase]
    loadAllSimpleTestCases = do
      testCaseNames <- discoverTestCaseNames
      mapM loadSimpleTestCase testCaseNames

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
