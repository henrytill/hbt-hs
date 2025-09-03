module Hbt.Formatter.HTMLTest (results) where

import Hbt.Formatter.HTML qualified as HTML
import Hbt.Parser.HTML qualified as HTMLParser
import Test.Dwergaz
import TestData (HtmlFormatterTestCase (..))
import TestUtilities (addContext, testResults)

name :: String
name = "Hbt.Formatter.HTML"

run :: HtmlFormatterTestCase -> Test
run testCase =
  let parsedOrFailed = HTMLParser.parse testCase.inputHtml
      formattedOrFailed = fmap (HTML.format testCase.template) (addContext "HTML parse failed" parsedOrFailed)
      assertExpected = assertEqual testCase.testName testCase.expectedHtml
   in either assertFailure assertExpected formattedOrFailed

allTests :: [HtmlFormatterTestCase] -> Test
allTests testData = group name (map run testData)

results :: [HtmlFormatterTestCase] -> (String, Bool)
results testData = testResults name (allTests testData)
