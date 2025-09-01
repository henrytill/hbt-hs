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

runTestSuite :: IO (String, Bool) -> IO (String, Bool)
runTestSuite suiteResults = do
  (output, passed) <- suiteResults
  putStr output
  pure (output, passed)

main :: IO ()
main = do
  testData <- loadAllTestData

  let testSuites =
        [ pure CollectionTest.results
        , pure $ HTMLFormatterTest.results testData.htmlFormatterTests
        , pure $ HTMLTest.results testData.htmlParserTests
        , pure $ MarkdownTest.results testData.markdownTests
        , pure $ PinboardJSONTest.results testData.pinboardJsonTests
        , pure $ PinboardXMLTest.results testData.pinboardXmlTests
        ]

  results <- mapM runTestSuite testSuites
  let allPassed = all snd results

  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
