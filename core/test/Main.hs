module Main (main) where

import Control.Monad (unless)
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.EntityProperties qualified as EntityProperties
import Hbt.Formatter.HTMLTest qualified as FormatterHTMLTest
import Hbt.Parser.HTMLTest qualified as ParserHTMLTest
import System.Exit (exitFailure)
import Test.QuickCheck (Property, isSuccess, quickCheckResult)
import Text.Printf (printf)

handleResults :: (String, Bool) -> IO Bool
handleResults (output, passed) = putStr output >> pure passed

runProps :: [(String, Property)] -> IO Bool
runProps props = do
  results <- traverse (\(name, prop) -> printf "Testing %s...\n" name >> quickCheckResult prop) props
  pure (all isSuccess results)

main :: IO ()
main = do
  testSuites <-
    sequence
      [ CollectionTest.results
      , ParserHTMLTest.results
      , FormatterHTMLTest.results
      ]

  results <- traverse handleResults testSuites
  propsPassed <- runProps EntityProperties.props
  let allPassed = and results && propsPassed

  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
