{-# LANGUAGE TemplateHaskell #-}

module Hbt.Formatter.HTMLTest where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Formatter.HTML qualified as HTML
import Hbt.Formatter.HTMLTest.TH (HtmlFormatterTestCase (..), loadAllHtmlFormatterTestDataTH)
import Hbt.Parser.HTML qualified as HTMLParser
import Test.Dwergaz

allTestData :: [HtmlFormatterTestCase]
allTestData = $(loadAllHtmlFormatterTestDataTH)

addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

-- | Normalize HTML by removing extra whitespace for comparison
normalizeHtml :: Text -> Text
normalizeHtml = Text.unlines . map Text.strip . Text.lines

runHtmlFormatterTestCase :: HtmlFormatterTestCase -> Test
runHtmlFormatterTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> pure (normalizeHtml testCase.expectedHtml)
      <*> (normalizeHtml . HTML.format <$> addContext "HTML parse failed" (HTMLParser.parse testCase.inputHtml))

allTests :: Test
allTests = group "Hbt.Formatter.HTML tests" (fmap runHtmlFormatterTestCase allTestData)

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
