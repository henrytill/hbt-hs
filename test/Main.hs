module Main (main) where

import Control.Monad (unless)
import Data.MultimapTest qualified as MultimapTest
import Hbt.Base.THTest qualified as THTest
import Hbt.BaseTest qualified as BaseTest
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.Formatter.HTMLTest qualified as HTMLFormatterTest
import Hbt.Parser.HTMLTest qualified as HTMLTest
import Hbt.Parser.MarkdownTest qualified as MarkdownTest
import Hbt.Parser.Pinboard.JSONTest qualified as PinboardJSONTest
import Hbt.Parser.Pinboard.XMLTest qualified as PinboardXMLTest
import System.Exit (exitFailure)
import TestData (AllTestData (..), loadAllTestData)
import Text.Printf (printf)

-- | Run a test suite and return its results
runTestSuite :: IO (String, Bool) -> IO (String, Bool)
runTestSuite suiteResults = do
  (output, passed) <- suiteResults
  putStr output
  return (output, passed)

main :: IO ()
main = do
  -- Load all test data once at startup
  testData <- loadAllTestData

  -- Define all test suites as IO actions
  let testSuites =
        [ return MultimapTest.results
        , return CollectionTest.results
        , return $ HTMLFormatterTest.results testData.htmlFormatterTests
        , return $ HTMLTest.results testData.htmlParserTests
        , return $ MarkdownTest.results testData.markdownTests
        , return $ PinboardJSONTest.results testData.pinboardJsonTests
        , return $ PinboardXMLTest.results testData.pinboardXmlTests
        , return BaseTest.results
        , return THTest.results
        ]

  -- Run all test suites and collect results
  results <- mapM runTestSuite testSuites
  let allPassed = all snd results

  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
