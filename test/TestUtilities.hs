module TestUtilities where

import Data.Bifunctor (first)
import Test.Dwergaz

-- | Add contextual information to error messages
addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first $ showString context . showString ": " . flip shows mempty

-- | Generate standardized test results with output and pass/fail status
testResults :: String -> Test -> (String, Bool)
testResults _groupName test =
  let result = runTest test
      buildString = showString (resultToString result) . showChar '\n'
      allPassed = resultIsPassed result
   in (buildString mempty, allPassed)
