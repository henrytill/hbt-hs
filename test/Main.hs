module Main (main) where

import Control.Monad (unless)
import Data.MultimapTest qualified as MultimapTest
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.Parser.HTMLTest qualified as HTMLTest
import Hbt.Parser.MarkdownTest qualified as MarkdownTest
import Hbt.Parser.Pinboard.JSONTest qualified as PinboardJSONTest
import Hbt.Parser.Pinboard.XMLTest qualified as PinboardXMLTest
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  let (multimapOutput, multimapPassed) = MultimapTest.results
  putStr multimapOutput
  let (collectionOutput, collectionPassed) = CollectionTest.results
  putStr collectionOutput
  let (htmlOutput, htmlPassed) = HTMLTest.results
  putStr htmlOutput
  let (markdownOutput, markdownPassed) = MarkdownTest.results
  putStr markdownOutput
  let (pinboardJsonOutput, pinboardJsonPassed) = PinboardJSONTest.results
  putStr pinboardJsonOutput
  let (pinboardXmlOutput, pinboardXmlPassed) = PinboardXMLTest.results
  putStr pinboardXmlOutput
  let allPassed = and [multimapPassed, collectionPassed, htmlPassed, markdownPassed, pinboardJsonPassed, pinboardXmlPassed]
  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
