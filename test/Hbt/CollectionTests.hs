module Hbt.CollectionTests where

import Test.Dwergaz

tests :: [Test]
tests = []

step :: Result -> (ShowS, Bool) -> (ShowS, Bool)
step result (f, allPassed) =
  ( showString (resultToString result) . showChar '\n' . f,
    resultIsPassed result && allPassed
  )

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    (buildString, allPassed) = foldr (step . runTest) (id, True) tests
