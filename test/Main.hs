module Main (main) where

import Control.Monad (unless)
import Data.MultimapTest qualified as MultimapTest
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.Formatter.HTMLTest qualified as HTMLFormatterTest
import Hbt.Parser.HTMLTest qualified as HTMLTest
import Hbt.Parser.MarkdownTest qualified as MarkdownTest
import Hbt.Parser.Pinboard.JSONTest qualified as PinboardJSONTest
import Hbt.Parser.Pinboard.XMLTest qualified as PinboardXMLTest
import System.Exit (exitFailure)
import TestData (AllTestData (..), loadAllTestData)
import Text.Printf (printf)

main :: IO ()
main = do
  -- Load all test data once at startup
  testData <- loadAllTestData

  let (multimapOutput, multimapPassed) = MultimapTest.results
  putStr multimapOutput
  let (collectionOutput, collectionPassed) = CollectionTest.results
  putStr collectionOutput
  let (htmlFormatterOutput, htmlFormatterPassed) = HTMLFormatterTest.results (testData.htmlFormatterTests)
  putStr htmlFormatterOutput
  let (htmlOutput, htmlPassed) = HTMLTest.results (testData.htmlParserTests)
  putStr htmlOutput
  let (markdownOutput, markdownPassed) = MarkdownTest.results (testData.markdownTests)
  putStr markdownOutput
  let (pinboardJsonOutput, pinboardJsonPassed) = PinboardJSONTest.results (testData.pinboardJsonTests)
  putStr pinboardJsonOutput
  let (pinboardXmlOutput, pinboardXmlPassed) = PinboardXMLTest.results (testData.pinboardXmlTests)
  putStr pinboardXmlOutput
  let allPassed = and [multimapPassed, collectionPassed, htmlFormatterPassed, htmlPassed, markdownPassed, pinboardJsonPassed, pinboardXmlPassed]
  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
