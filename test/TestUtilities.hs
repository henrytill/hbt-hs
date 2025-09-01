module TestUtilities where

import Data.Bifunctor (first)
import Test.Dwergaz

-- | Add contextual information to error messages
addContext :: (Show e) => String -> Either e a -> Either String a
addContext context = first (\e -> showString context (showString ": " (shows e mempty)))

-- | Generate standardized test results with output and pass/fail status
testResults :: String -> Test -> (String, Bool)
testResults _groupName test =
  let result = runTest test
      buildString output = showString (resultToString result) (showChar '\n' output)
      allPassed = resultIsPassed result
   in (buildString mempty, allPassed)
