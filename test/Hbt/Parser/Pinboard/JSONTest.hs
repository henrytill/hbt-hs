module Hbt.Parser.Pinboard.JSONTest where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Test.Dwergaz
import TestData (PinboardParserTestCase (..))
import TestUtilities (testIO, testResults)

runPinboardJSONTestCase :: PinboardParserTestCase -> IO Test
runPinboardJSONTestCase testCase = testIO testCase.testName $ do
  expected <- Yaml.decodeThrow (Text.Encoding.encodeUtf8 testCase.expectedYaml)
  actual <- PinboardJSON.parse testCase.inputText
  pure (assertEqual testCase.testName expected actual)

allTests :: [PinboardParserTestCase] -> IO Test
allTests testData = do
  tests <- traverse runPinboardJSONTestCase testData
  pure (group "Hbt.Parser.Pinboard.JSON tests" tests)

results :: [PinboardParserTestCase] -> IO (String, Bool)
results testData = do
  test <- allTests testData
  pure (testResults "Hbt.Parser.Pinboard.JSON" test)
