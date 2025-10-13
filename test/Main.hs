module Main (main) where

import Control.Monad (unless)
import Hbt (Format (..))
import Hbt.CollectionTest qualified as CollectionTest
import System.Exit (exitFailure)
import TestData (discoverInput, discoverOutput, formatterTests, parserTests)
import TestUtilities (testResults)
import Text.Printf (printf)

handleResults :: (String, Bool) -> IO Bool
handleResults (output, passed) = putStr output >> pure passed

main :: IO ()
main = do
  htmlParserCases <- discoverInput HTML
  htmlFormatterCases <- discoverOutput HTML
  markdownParserCases <- discoverInput Markdown
  jsonParserCases <- discoverInput JSON
  xmlParserCases <- discoverInput XML
  testSuites <-
    sequence
      [ pure CollectionTest.results
      , testResults "Hbt.Parser.HTML" <$> parserTests "HTML Parser" htmlParserCases
      , testResults "Hbt.Formatter.HTML" <$> formatterTests "HTML Formatter" HTML htmlFormatterCases
      , testResults "Hbt.Parser.Markdown" <$> parserTests "Markdown Parser" markdownParserCases
      , testResults "Hbt.Parser.Pinboard.JSON" <$> parserTests "Pinboard JSON Parser" jsonParserCases
      , testResults "Hbt.Parser.Pinboard.XML" <$> parserTests "Pinboard XML Parser" xmlParserCases
      ]

  results <- traverse handleResults testSuites
  let allPassed = and results

  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
