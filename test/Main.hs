module Main (main) where

import Control.Monad (unless)
import Data.MultimapTest qualified as MultimapTest
import Hbt.CollectionTest qualified as CollectionTest
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  let (multimapOutput, multimapPassed) = MultimapTest.results
  putStr multimapOutput
  let (collectionOutput, collectionPassed) = CollectionTest.results
  putStr collectionOutput
  let allPassed = and [multimapPassed, collectionPassed]
  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
