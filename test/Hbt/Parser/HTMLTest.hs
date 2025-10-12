module Hbt.Parser.HTMLTest (results) where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.HTML qualified as HTML
import Test.Dwergaz
import TestData (HtmlParserTestCase (..))
import TestUtilities (testIO, testResults)

name :: String
name = "Hbt.Parser.HTML"

run :: HtmlParserTestCase -> IO Test
run testCase = testIO testCase.testName $ do
  expected <- Yaml.decodeThrow (Text.Encoding.encodeUtf8 testCase.expectedYaml)
  actual <- HTML.parse testCase.inputHtml
  pure (assertEqual testCase.testName expected actual)

allTests :: [HtmlParserTestCase] -> IO Test
allTests testData = do
  tests <- traverse run testData
  pure (group name tests)

results :: [HtmlParserTestCase] -> IO (String, Bool)
results testData = do
  test <- allTests testData
  pure (testResults name test)
