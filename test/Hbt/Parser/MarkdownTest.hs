module Hbt.Parser.MarkdownTest (results) where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Markdown qualified as Markdown
import Test.Dwergaz
import TestData (MarkdownParserTestCase (..))
import TestUtilities (testIO, testResults)

name :: String
name = "Hbt.Parser.Markdown"

run :: MarkdownParserTestCase -> IO Test
run testCase = testIO testCase.testName $ do
  expected <- Yaml.decodeThrow (Text.Encoding.encodeUtf8 testCase.expectedYaml)
  actual <- Markdown.parse testCase.testName testCase.inputMarkdown
  pure (assertEqual testCase.testName expected actual)

allTests :: [MarkdownParserTestCase] -> IO Test
allTests testData = do
  tests <- traverse run testData
  pure (group name tests)

results :: [MarkdownParserTestCase] -> IO (String, Bool)
results testData = do
  test <- allTests testData
  pure (testResults name test)
