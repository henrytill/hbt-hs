module Data.MultimapTest where

import Data.Multimap qualified as Multimap
import Data.Set qualified as Set
import Test.Dwergaz

testKey1 :: String
testKey1 = "one"

testKey2 :: String
testKey2 = "two"

empty :: Multimap.Multimap String Int
empty = Multimap.empty

inserter :: (Ord k, Ord v) => (k, v) -> Multimap.Multimap k v -> Multimap.Multimap k v
inserter (k, v) = Multimap.insert k v

emptyIsNull :: Test
emptyIsNull =
  assertBool
    "null returns true on empty map"
    (Multimap.null Multimap.empty)

nonEmptyIsNotNull :: Test
nonEmptyIsNotNull =
  assertBool
    "null returns false on non-empty map"
    (not . Multimap.null $ Multimap.insert testKey1 1 empty)

nonEmptyHasItem :: Test
nonEmptyHasItem =
  assertEqual
    "non-empty map has inserted item"
    (Multimap.lookup testKey1 testMap)
    (Set.fromList [1])
  where
    testMap = foldr inserter empty [(testKey1, 1)]

twoValuesUnderKey :: Test
twoValuesUnderKey =
  assertEqual
    "two values can be stored under the same key"
    (Multimap.lookup testKey1 testMap)
    (Set.fromList [1, 2])
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2)]

deleteAllWorks :: Test
deleteAllWorks =
  assertEqual
    "deleteAll removes all values under a key"
    (Multimap.lookup testKey1 (Multimap.deleteAll testKey1 testMap))
    Set.empty
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2)]

deleteWorks :: Test
deleteWorks =
  assertEqual
    "delete removes a specific value under a key"
    (Multimap.lookup testKey1 (Multimap.delete testKey1 1 testMap))
    (Set.fromList [2])
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2)]

fromListWorks :: Test
fromListWorks =
  assertEqual
    "fromList constructs a map from a list of pairs"
    (Multimap.fromList [(testKey1, 1), (testKey1, 2)])
    (foldr inserter empty [(testKey1, 1), (testKey1, 2)])

elemsWorks :: Test
elemsWorks =
  assertEqual
    "elems returns all values in the map"
    (Multimap.elems testMap)
    [1, 2, 3]
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2), (testKey2, 3)]

allTests :: Test
allTests =
  group
    "Multimap tests"
    [ emptyIsNull,
      nonEmptyIsNotNull,
      nonEmptyHasItem,
      twoValuesUnderKey,
      deleteAllWorks,
      deleteWorks,
      fromListWorks,
      elemsWorks
    ]

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString (resultToString result)
    buildString = showResults . showChar '\n'
