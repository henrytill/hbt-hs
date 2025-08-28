{-# LANGUAGE TemplateHaskell #-}

module Hbt.Parser.Pinboard.XMLTest where

import Data.Bifunctor (first)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Pinboard.TH (PinboardTestCase (..), loadAllPinboardTestDataTH)
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import Test.Dwergaz

allTestData :: [PinboardTestCase]
allTestData = $(loadAllPinboardTestDataTH)

xmlTestData :: [PinboardTestCase]
xmlTestData = filter (\tc -> tc.format == "xml") allTestData

addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

runPinboardXMLTestCase :: PinboardTestCase -> Test
runPinboardXMLTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (PinboardXML.parse testCase.inputText)

allTests :: Test
allTests = group "Hbt.Parser.Pinboard.XML tests" (fmap runPinboardXMLTestCase xmlTestData)

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
