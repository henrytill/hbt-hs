module Hbt.Parser.MarkdownTest (results) where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Markdown qualified as Markdown
import Test.Dwergaz
import TestData (MarkdownParserTestCase (..))
import TestUtilities (addContext, testResults)

name :: String
name = "Hbt.Parser.Markdown"

run :: MarkdownParserTestCase -> Test
run testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (Markdown.parse testCase.testName testCase.inputMarkdown)

allTests :: [MarkdownParserTestCase] -> Test
allTests testData = group name (map run testData)

results :: [MarkdownParserTestCase] -> (String, Bool)
results testData = testResults name (allTests testData)
