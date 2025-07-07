module Main (main) where

import Commonmark.InitialTest qualified as InitialTest
import Control.Monad (unless)
import Data.MultimapTest qualified as MultimapTest
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.MarkdownTest qualified as MarkdownTest
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  let (multimapOutput, multimapPassed) = MultimapTest.results
  putStr multimapOutput
  let (collectionOutput, collectionPassed) = CollectionTest.results
  putStr collectionOutput
  let (markdownOutput, markdownPassed) = MarkdownTest.results
  putStr markdownOutput
  let (initialOutput, initialPassed) = InitialTest.results
  putStr initialOutput
  let allPassed = and [multimapPassed, collectionPassed, markdownPassed, initialPassed]
  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
