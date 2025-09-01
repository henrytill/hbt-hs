module Hbt.Parser.Pinboard.JSONTest where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Test.Dwergaz
import TestData (PinboardTestCase (..))
import TestUtilities (addContext, testResults)

runPinboardJSONTestCase :: PinboardTestCase -> Test
runPinboardJSONTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (PinboardJSON.parse testCase.inputText)

allTests :: [PinboardTestCase] -> Test
allTests testData = group "Hbt.Parser.Pinboard.JSON tests" (map runPinboardJSONTestCase testData)

results :: [PinboardTestCase] -> (String, Bool)
results testData = testResults "Hbt.Parser.Pinboard.JSON" (allTests testData)
