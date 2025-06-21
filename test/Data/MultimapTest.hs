module Data.MultimapTest where

import Data.Foldable qualified as Foldable
import Data.Multimap (Multimap)
import Data.Multimap qualified as Multimap
import Data.Set qualified as Set
import Test.Dwergaz

a, b, c :: String
a = "a"
b = "b"
c = "c"

emptyTests :: Test
emptyTests =
  group
    "Empty multimap tests"
    [ assertBool "null returns true on empty map" $ Multimap.null testMap
    , assertEqual "empty lookup" Set.empty $ Multimap.lookup a testMap
    , assertEqual "empty size" 0 $ Multimap.size testMap
    , assertEqual "empty elems" [] $ Multimap.elems testMap
    ]
  where
    testMap :: Multimap String Int
    testMap = Multimap.empty

insertTests :: Test
insertTests =
  group
    "Insert and lookup tests"
    [ assertBool "null returns false on non-empty map" . not . Multimap.null . Multimap.insert a 1 $ Multimap.empty @String @Int
    , assertEqual "single inserted item" (Set.singleton 1) (Multimap.lookup a singleMap)
    , assertEqual "two values under one key" (Set.fromList [1, 2]) (Multimap.lookup a doubleMap)
    , assertEqual "non-existent key" Set.empty $ Multimap.lookup c doubleMap
    , assertEqual "first key" (Set.fromList [1, 2]) (Multimap.lookup a multiKeyMap)
    , assertEqual "second key" (Set.fromList [3, 4]) (Multimap.lookup b multiKeyMap)
    ]
  where
    singleMap, doubleMap, multiKeyMap :: Multimap String Int
    singleMap = Multimap.insert a 1 Multimap.empty
    doubleMap = Multimap.insert a 2 . Multimap.insert a 1 $ Multimap.empty
    multiKeyMap = Multimap.fromList [(a, 1), (a, 2), (b, 3), (b, 4)]

deleteTests :: Test
deleteTests =
  group
    "Delete tests"
    [ assertEqual "deleteAll result" Set.empty . Multimap.lookup a . Multimap.deleteAll a $ testMap
    , assertEqual "deleteAll preserve" (Set.singleton 3) (Multimap.lookup b . Multimap.deleteAll a $ testMap)
    , assertEqual "delete specific" (Set.singleton 2) (Multimap.lookup a . Multimap.delete a 1 $ testMap)
    , assertEqual "delete preserves" (Set.singleton 3) (Multimap.lookup b . Multimap.delete a 1 $ testMap)
    , assertEqual "delete non-existent key" testMap $ Multimap.delete c 1 testMap
    , assertEqual "delete non-existent value" testMap $ Multimap.delete a 99 testMap
    ]
  where
    testMap :: Multimap String Int
    testMap = Multimap.fromList [(a, 1), (a, 2), (b, 3)]

constructionTests :: Test
constructionTests =
  group
    "Construction tests"
    [ assertEqual "fromList construction" expectedMap $ Multimap.fromList testPairs
    , assertEqual "fromList duplicates" (Set.fromList @Int [1, 1, 2]) (Multimap.lookup a (Multimap.fromList [(a, 1), (a, 1), (a, 2)]))
    ]
  where
    testPairs :: [(String, Int)]
    testPairs = [(a, 1), (a, 2), (b, 3)]
    expectedMap = Multimap.fromList testPairs

sizeElemsTests :: Test
sizeElemsTests =
  group
    "Size and elements tests"
    [ assertEqual "elems result" [1, 2, 3] $ Multimap.elems testMap
    , assertEqual "size result" 3 $ Multimap.size testMap
    , assertEqual "size matches elems length" (length $ Multimap.elems testMap) (Multimap.size testMap)
    ]
  where
    testMap :: Multimap String Int
    testMap = Multimap.fromList [(a, 1), (a, 2), (b, 3)]

foldTests :: Test
foldTests =
  group
    "Fold tests"
    [ assertEqual
        "foldr result"
        [Set.fromList [1, 2], Set.singleton 3]
        (Multimap.foldr (:) [] testMap)
    , assertEqual
        "foldl result"
        [Set.singleton 3, Set.fromList [1, 2]]
        (Multimap.foldl (flip (:)) [] testMap)
    , assertEqual
        "foldrWithKey result"
        [(a, Set.fromList [1, 2]), (b, Set.singleton 3)]
        (Multimap.foldrWithKey (\k v acc -> (k, v) : acc) [] testMap)
    , assertEqual
        "foldlWithKey result"
        [(b, Set.singleton 3), (a, Set.fromList [1, 2])]
        (Multimap.foldlWithKey (\acc k v -> (k, v) : acc) [] testMap)
    , assertEqual
        "foldMapWithKey collects all values"
        (Set.fromList [1, 2, 3])
        (Multimap.foldMapWithKey (\_ v -> v) testMap)
    ]
  where
    testMap :: Multimap String Int
    testMap = Multimap.fromList [(a, 1), (a, 2), (b, 3)]

unionTests :: Test
unionTests =
  group
    "Union tests"
    [ assertEqual "union result" (Set.fromList [1, 2]) (Multimap.lookup a unionResult)
    , assertEqual "union unique keys" (Set.singleton 4) (Multimap.lookup b unionResult)
    , assertEqual "unions result" expectedUnions $ Multimap.unions [map1, map2, map3]
    ]
  where
    map1, map2, map3, unionResult, expectedUnions :: Multimap String Int
    map1 = Multimap.fromList [(a, 1), (b, 4)]
    map2 = Multimap.fromList [(a, 2)]
    map3 = Multimap.fromList [(a, 3), (c, 5)]
    unionResult = Multimap.union map1 map2
    expectedUnions = Multimap.fromList [(a, 1), (a, 2), (a, 3), (b, 4), (c, 5)]

semigroupMonoidTests :: Test
semigroupMonoidTests =
  group
    "Semigroup and Monoid laws"
    [ assertEqual "associativity" ((map1 <> map2) <> map3) (map1 <> (map2 <> map3))
    , assertEqual "left identity" map1 $ mempty <> map1
    , assertEqual "right identity" map1 $ map1 <> mempty
    , assertEqual "mconcat" (Foldable.fold [map1, map2, map3]) (mconcat [map1, map2, map3])
    ]
  where
    map1, map2, map3 :: Multimap String Int
    map1 = Multimap.fromList [(a, 1)]
    map2 = Multimap.fromList [(a, 2)]
    map3 = Multimap.fromList [(b, 3)]

edgeCaseTests :: Test
edgeCaseTests =
  group
    "Edge cases"
    [ assertEqual "insert-delete identity" Multimap.empty . Multimap.delete a 1 $ Multimap.insert a 1 empty
    , assertBool "key removal on last value" . Multimap.null . Multimap.delete a 1 $ Multimap.insert a 1 empty
    , assertEqual "union with empty" map1 $ Multimap.union map1 empty
    , assertEqual "size reduction" 1 . Multimap.size . Multimap.delete a 1 $ Multimap.fromList @String @Int [(a, 1), (a, 2)]
    ]
  where
    empty, map1 :: Multimap String Int
    empty = Multimap.empty
    map1 = Multimap.fromList [(a, 1), (b, 2)]

allTests :: Test
allTests =
  group
    "Data.Multimap tests"
    [ emptyTests
    , insertTests
    , deleteTests
    , constructionTests
    , sizeElemsTests
    , foldTests
    , unionTests
    , semigroupMonoidTests
    , edgeCaseTests
    ]

-- Run tests
results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
