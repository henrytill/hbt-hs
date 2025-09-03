module Hbt.Parser.HTMLTest (results) where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.HTML qualified as HTML
import Test.Dwergaz
import TestData (HtmlParserTestCase (..))
import TestUtilities (addContext, testResults)

name :: String
name = "Hbt.Parser.HTML"

run :: HtmlParserTestCase -> Test
run testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (HTML.parse testCase.inputHtml)

allTests :: [HtmlParserTestCase] -> Test
allTests testData = group name (map run testData)

results :: [HtmlParserTestCase] -> (String, Bool)
results testData = testResults name (allTests testData)
