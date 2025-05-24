{-# LANGUAGE OverloadedStrings #-}

module Commonmark.InitialTest where

import Commonmark
import Commonmark.Initial
import Data.Either qualified as Either
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Dwergaz

parse :: Text -> Either ParseError Blocks
parse = commonmark "test"

testHeading :: Test
testHeading = assertBool "Test Heading" . Either.isRight $ parse input
  where
    input =
      Text.unlines
        ["# Hello, world!"]

allTests :: Test
allTests =
  group
    "Commonmark.Initial tests"
    [testHeading]

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString (resultToString result)
    buildString = showResults . showChar '\n'
