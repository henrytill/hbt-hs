module TestUtilities
  ( testIO
  , testResults
  , safeURI
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Test.Dwergaz

-- | Run an IO action that produces a Test, catching any exceptions as test failures
testIO :: String -> IO Test -> IO Test
testIO testName action = do
  result <- try action
  pure $ case result of
    Left (e :: SomeException) -> assertFailure (testName ++ ": " ++ show e)
    Right test -> test

-- | Generate standardized test results with output and pass/fail status
testResults :: String -> Test -> (String, Bool)
testResults _groupName test = (buildString mempty, allPassed)
  where
    result = runTest test
    buildString output = showString (resultToString result) (showChar '\n' output)
    allPassed = resultIsPassed result

-- | Parse a URI the test states, failing loudly if it does not parse
safeURI :: Text -> URI
safeURI = either (error . show) id . URI.parse
