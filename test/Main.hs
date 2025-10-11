module Main (main) where

import Control.Monad (unless)
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.Formatter.HTMLTest qualified as HTMLFormatterTest
import Hbt.Parser.HTMLTest qualified as HTMLTest
import Hbt.Parser.MarkdownTest qualified as MarkdownTest
import Hbt.Parser.Pinboard.JSONTest qualified as PinboardJSONTest
import Hbt.Parser.Pinboard.XMLTest qualified as PinboardXMLTest
import System.Exit (exitFailure)
import TestData (AllTestData (..), loadAllTestData)
import Text.Printf (printf)

handleResults :: (String, Bool) -> IO Bool
handleResults (output, passed) = putStr output >> pure passed

main :: IO ()
main = do
  testData <- loadAllTestData
  testSuites <-
    sequence
      [ pure CollectionTest.results
      , HTMLFormatterTest.results testData.htmlFormatterTests
      , HTMLTest.results testData.htmlParserTests
      , MarkdownTest.results testData.markdownTests
      , PinboardJSONTest.results testData.pinboardJsonTests
      , PinboardXMLTest.results testData.pinboardXmlTests
      ]
  results <- traverse handleResults testSuites
  let allPassed = and results
  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
