{-# LANGUAGE TemplateHaskell #-}

module Hbt.Parser.MarkdownTest where

import Data.Bifunctor (first)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Markdown qualified as Markdown
import Hbt.Parser.MarkdownTest.TH (SimpleTestCase (..), loadAllTestDataTH)
import Test.Dwergaz

allTestData :: [SimpleTestCase]
allTestData = $(loadAllTestDataTH)

addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

runSimpleTestCase :: SimpleTestCase -> Test
runSimpleTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (Markdown.parse testCase.testName testCase.inputMarkdown)

allTests :: Test
allTests = group "Hbt.Markdown tests" (fmap runSimpleTestCase allTestData)

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
