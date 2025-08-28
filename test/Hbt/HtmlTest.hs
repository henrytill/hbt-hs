{-# LANGUAGE TemplateHaskell #-}

module Hbt.HtmlTest where

import Data.Bifunctor (first)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Html.Netscape qualified as Html
import Hbt.HtmlTest.TH (HtmlTestCase (..), loadAllHtmlTestDataTH)
import Test.Dwergaz

allTestData :: [HtmlTestCase]
allTestData = $(loadAllHtmlTestDataTH)

addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

runHtmlTestCase :: HtmlTestCase -> Test
runHtmlTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (Html.parse testCase.inputHtml)

allTests :: Test
allTests = group "Hbt.Html tests" (fmap runHtmlTestCase allTestData)

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
