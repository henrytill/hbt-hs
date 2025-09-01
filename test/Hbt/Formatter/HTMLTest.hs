module Hbt.Formatter.HTMLTest where

import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Formatter.HTML qualified as HTML
import Hbt.Parser.HTML qualified as HTMLParser
import Test.Dwergaz
import TestData (HtmlFormatterTestCase (..))
import TestUtilities (addContext, testResults)

normalizeHtml :: Text -> Text
normalizeHtml text = Text.unlines (map Text.strip (Text.lines text))

runHtmlFormatterTestCase :: HtmlFormatterTestCase -> Test
runHtmlFormatterTestCase testCase =
  either
    assertFailure
    ( \test ->
        let normalizedExpected = normalizeHtml testCase.expectedHtml
            normalizedObtained = normalizeHtml (HTML.format testCase.template test)
         in assertEqual testCase.testName normalizedExpected normalizedObtained
    )
    (addContext "HTML parse failed" (HTMLParser.parse testCase.inputHtml))

allTests :: [HtmlFormatterTestCase] -> Test
allTests testData = group "Hbt.Formatter.HTML tests" (map runHtmlFormatterTestCase testData)

results :: [HtmlFormatterTestCase] -> (String, Bool)
results testData = testResults "Hbt.Formatter.HTML" (allTests testData)
