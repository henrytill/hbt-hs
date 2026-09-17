module Main (main) where

import Control.Monad (unless)
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.Formatter.HTMLTest qualified as FormatterHTMLTest
import Hbt.Parser.HTMLTest qualified as ParserHTMLTest
import System.Exit (exitFailure)
import Text.Printf (printf)

handleResults :: (String, Bool) -> IO Bool
handleResults (output, passed) = putStr output >> pure passed

main :: IO ()
main = do
  testSuites <-
    sequence
      [ CollectionTest.results
      , ParserHTMLTest.results
      , FormatterHTMLTest.results
      ]

  results <- traverse handleResults testSuites
  let allPassed = and results

  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
