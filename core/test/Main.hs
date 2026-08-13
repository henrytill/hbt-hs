module Main (main) where

import Control.Monad (unless)
import Hbt (Format (..))
import Hbt.CollectionTest qualified as CollectionTest
import Hbt.Formatter.HTMLTest qualified as FormatterHTMLTest
import Hbt.Parser.HTMLTest qualified as ParserHTMLTest
import System.Exit (exitFailure)
import TestData (discoverInput, discoverOutput, formatterTests, parserTests)
import TestUtilities (testResults)
import Text.Printf (printf)

handleResults :: (String, Bool) -> IO Bool
handleResults (output, passed) = putStr output >> pure passed

main :: IO ()
main = do
  htmlParserCases <- discoverInput HTML
  htmlFormatterCases <- discoverOutput HTML HTML
  htmlYamlCases <- discoverOutput HTML YAML
  markdownYamlCases <- discoverOutput Markdown YAML
  jsonYamlCases <- discoverOutput JSON YAML
  xmlYamlCases <- discoverOutput XML YAML
  markdownParserCases <- discoverInput Markdown
  jsonParserCases <- discoverInput JSON
  xmlParserCases <- discoverInput XML
  testSuites <-
    sequence
      [ CollectionTest.results
      , testResults "Hbt.Parser.HTML" <$> parserTests "HTML Parser" htmlParserCases
      , testResults "Hbt.Formatter.HTML" <$> formatterTests "HTML Formatter" HTML htmlFormatterCases
      , ParserHTMLTest.results
      , FormatterHTMLTest.results
      , testResults "Hbt.Parser.Markdown" <$> parserTests "Markdown Parser" markdownParserCases
      , testResults "Hbt.Parser.Pinboard.JSON" <$> parserTests "Pinboard JSON Parser" jsonParserCases
      , testResults "Hbt.Parser.Pinboard.XML" <$> parserTests "Pinboard XML Parser" xmlParserCases
      , testResults "Hbt.Formatter.YAML" <$> formatterTests "YAML Formatter (html)" HTML htmlYamlCases
      , testResults "Hbt.Formatter.YAML" <$> formatterTests "YAML Formatter (markdown)" Markdown markdownYamlCases
      , testResults "Hbt.Formatter.YAML" <$> formatterTests "YAML Formatter (pinboard json)" JSON jsonYamlCases
      , testResults "Hbt.Formatter.YAML" <$> formatterTests "YAML Formatter (pinboard xml)" XML xmlYamlCases
      ]

  results <- traverse handleResults testSuites
  let allPassed = and results

  printf "Summary: %s\n" (if allPassed then "All tests passed!" else "Some tests failed.")
  unless allPassed exitFailure
