module Hbt.Parser.HTMLTest where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.HTML qualified as HTML
import Test.Dwergaz
import TestData (HtmlTestCase (..))
import TestUtilities (addContext, testResults)

runHtmlTestCase :: HtmlTestCase -> Test
runHtmlTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (HTML.parse testCase.inputHtml)

allTests :: [HtmlTestCase] -> Test
allTests testData = group "Hbt.Html tests" (map runHtmlTestCase testData)

results :: [HtmlTestCase] -> (String, Bool)
results testData = testResults "Hbt.Parser.HTML" (allTests testData)
