module HbtTest where

import Test.Dwergaz
import TestUtilities (testResults)

allTests :: Test
allTests =
  group
    "Hbt"
    []

-- Run tests
results :: (String, Bool)
results = testResults "Hbt" allTests
