module Hbt.Parser.Pinboard.XMLTest where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import Test.Dwergaz
import TestData (PinboardParserTestCase (..))
import TestUtilities (addContext, testResults)

runPinboardXMLTestCase :: PinboardParserTestCase -> Test
runPinboardXMLTestCase testCase =
  either assertFailure id $
    assertEqual testCase.testName
      <$> addContext "YAML decode failed" (Yaml.decodeEither' (Text.Encoding.encodeUtf8 testCase.expectedYaml))
      <*> addContext "Parse failed" (PinboardXML.parse testCase.inputText)

allTests :: [PinboardParserTestCase] -> Test
allTests testData = group "Hbt.Parser.Pinboard.XML tests" (map runPinboardXMLTestCase testData)

results :: [PinboardParserTestCase] -> (String, Bool)
results testData = testResults "Hbt.Parser.Pinboard.XML" (allTests testData)
