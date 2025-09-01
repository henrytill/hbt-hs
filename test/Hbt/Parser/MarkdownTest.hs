module Hbt.Parser.MarkdownTest where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Markdown qualified as Markdown
import Test.Dwergaz
import TestData (SimpleTestCase (..))
import TestUtilities (addContext, testResults)

runSimpleTestCase :: SimpleTestCase -> Test
runSimpleTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (Markdown.parse testCase.testName testCase.inputMarkdown)

allTests :: [SimpleTestCase] -> Test
allTests testData = group "Hbt.Markdown tests" (map runSimpleTestCase testData)

results :: [SimpleTestCase] -> (String, Bool)
results testData = testResults "Hbt.Parser.Markdown" (allTests testData)
