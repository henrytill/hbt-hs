module Hbt.Parser.HTMLTest where

import Data.Bifunctor (first)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.HTML qualified as HTML
import Test.Dwergaz
import TestData (HtmlTestCase (..))

addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

runHtmlTestCase :: HtmlTestCase -> Test
runHtmlTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (HTML.parse testCase.inputHtml)

allTests :: [HtmlTestCase] -> Test
allTests testData = group "Hbt.Html tests" (fmap runHtmlTestCase testData)

results :: [HtmlTestCase] -> (String, Bool)
results testData = (buildString mempty, allPassed)
  where
    result = runTest (allTests testData)
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
