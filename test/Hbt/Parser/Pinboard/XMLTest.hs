module Hbt.Parser.Pinboard.XMLTest where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import Test.Dwergaz
import TestData (PinboardParserTestCase (..))
import TestUtilities (testIO, testResults)

runPinboardXMLTestCase :: PinboardParserTestCase -> IO Test
runPinboardXMLTestCase testCase = testIO testCase.testName $ do
  expected <- Yaml.decodeThrow (Text.Encoding.encodeUtf8 testCase.expectedYaml)
  actual <- PinboardXML.parse testCase.inputText
  pure (assertEqual testCase.testName expected actual)

allTests :: [PinboardParserTestCase] -> IO Test
allTests testData = do
  tests <- traverse runPinboardXMLTestCase testData
  pure (group "Hbt.Parser.Pinboard.XML tests" tests)

results :: [PinboardParserTestCase] -> IO (String, Bool)
results testData = do
  test <- allTests testData
  pure (testResults "Hbt.Parser.Pinboard.XML" test)
