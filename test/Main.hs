module Main (main) where

import Control.Monad (unless)
import Data.MultimapTest qualified as MultimapTest
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.HtmlTest qualified as HtmlTest
import Hbt.MarkdownTest qualified as MarkdownTest
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  let (multimapOutput, multimapPassed) = MultimapTest.results
  putStr multimapOutput
  let (collectionOutput, collectionPassed) = CollectionTest.results
  putStr collectionOutput
  let (htmlOutput, htmlPassed) = HtmlTest.results
  putStr htmlOutput
  let (markdownOutput, markdownPassed) = MarkdownTest.results
  putStr markdownOutput
  let allPassed = and [multimapPassed, collectionPassed, htmlPassed, markdownPassed]
  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
