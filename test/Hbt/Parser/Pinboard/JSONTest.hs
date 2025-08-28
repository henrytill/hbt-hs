{-# LANGUAGE TemplateHaskell #-}

module Hbt.Parser.Pinboard.JSONTest where

import Data.Bifunctor (first)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Hbt.Parser.Pinboard.TH (PinboardTestCase (..), loadAllPinboardTestDataTH)
import Test.Dwergaz

allTestData :: [PinboardTestCase]
allTestData = $(loadAllPinboardTestDataTH)

jsonTestData :: [PinboardTestCase]
jsonTestData = filter (\tc -> tc.format == "json") allTestData

addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

runPinboardJSONTestCase :: PinboardTestCase -> Test
runPinboardJSONTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (PinboardJSON.parse testCase.inputText)

allTests :: Test
allTests = group "Hbt.Parser.Pinboard.JSON tests" (fmap runPinboardJSONTestCase jsonTestData)

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
