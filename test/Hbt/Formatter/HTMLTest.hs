module Hbt.Formatter.HTMLTest (results) where

import Hbt.Formatter.HTML qualified as HTML
import Hbt.Parser.HTML qualified as HTMLParser
import Test.Dwergaz
import TestData (HtmlFormatterTestCase (..))
import TestUtilities (testIO, testResults)

name :: String
name = "Hbt.Formatter.HTML"

run :: HtmlFormatterTestCase -> IO Test
run testCase = testIO testCase.testName $ do
  parsed <- HTMLParser.parse testCase.inputHtml
  pure (assertEqual testCase.testName testCase.expectedHtml (HTML.format testCase.template parsed))

allTests :: [HtmlFormatterTestCase] -> IO Test
allTests testData = do
  tests <- traverse run testData
  pure (group name tests)

results :: [HtmlFormatterTestCase] -> IO (String, Bool)
results testData = do
  test <- allTests testData
  pure (testResults name test)
