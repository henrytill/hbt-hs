{-# LANGUAGE OverloadedStrings #-}

module Hbt.CollectionTests where

import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Vector ((!))
import Data.Vector qualified as Vector
import Hbt.Collection
import Network.URI (URI)
import Network.URI qualified as URI
import Test.Dwergaz
import Prelude hiding (id, length, null)

mkURI :: String -> URI
mkURI s = Maybe.fromMaybe (error $ "Invalid URI: " ++ s) (URI.parseURI s)

mkTime :: Integer -> Time
mkTime = MkTime . fromIntegral

emptyEntityTests :: Test
emptyEntityTests =
  group
    "Entity operations on empty entity"
    [ assertEqual "emptyEntity has null URI" URI.nullURI (entityUri emptyEntity),
      assertEqual "emptyEntity has empty creation time" emptyTime (entityCreatedAt emptyEntity),
      assertEqual "emptyEntity has empty update history" [] (entityUpdatedAt emptyEntity),
      assertEqual "emptyEntity has empty names" Set.empty (entityNames emptyEntity),
      assertEqual "emptyEntity has empty labels" Set.empty (entityLabels emptyEntity)
    ]

entityTests :: Test
entityTests =
  let uri = mkURI "https://example.com"
      time = mkTime 1000
      name = Just $ MkName "Test Entity"
      labels = Set.fromList [MkLabel "label1", MkLabel "label2"]
      entity = mkEntity uri time name labels
      expectedNames = Set.singleton (MkName "Test Entity")
   in group
        "Entity operations"
        [ assertEqual "mkEntity sets correct URI" uri (entityUri entity),
          assertEqual "mkEntity sets correct creation time" time (entityCreatedAt entity),
          assertEqual "mkEntity sets empty update history" [] (entityUpdatedAt entity),
          assertEqual "mkEntity sets correct names" expectedNames (entityNames entity),
          assertEqual "mkEntity sets correct labels" labels (entityLabels entity)
        ]

updateEntityTests :: Test
updateEntityTests =
  let baseEntity =
        mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Original")
          (Set.singleton $ MkLabel "label1")
      updateTime = mkTime 2000
      updateNames = Set.singleton $ MkName "Updated"
      updateLabels = Set.singleton $ MkLabel "label2"
      updatedEntity = updateEntity updateTime updateNames updateLabels baseEntity
      expectedNames = Set.fromList [MkName "Original", MkName "Updated"]
      expectedLabels = Set.fromList [MkLabel "label1", MkLabel "label2"]
      expectedUpdates = [mkTime 2000]

      olderBaseEntity =
        mkEntity
          (mkURI "https://example.com")
          (mkTime 2000)
          (Just $ MkName "Original")
          (Set.singleton $ MkLabel "label1")
      olderUpdateTime = mkTime 1000
      olderUpdatedEntity = updateEntity olderUpdateTime updateNames updateLabels olderBaseEntity
      olderExpectedNames = Set.fromList [MkName "Original", MkName "Updated"]
      olderExpectedLabels = Set.fromList [MkLabel "label1", MkLabel "label2"]
   in group
        "Entity update operations"
        [ assertEqual "updateEntity preserves creation time when update is newer" (mkTime 1000) (entityCreatedAt updatedEntity),
          assertEqual "updateEntity adds update history when update is newer" expectedUpdates (entityUpdatedAt updatedEntity),
          assertEqual "updateEntity merges names when update is newer" expectedNames (entityNames updatedEntity),
          assertEqual "updateEntity merges labels when update is newer" expectedLabels (entityLabels updatedEntity),
          assertEqual "updateEntity changes creation time when update is older" olderUpdateTime (entityCreatedAt olderUpdatedEntity),
          assertBool "updateEntity preserves original time in history when update is older" $ mkTime 2000 `elem` entityUpdatedAt olderUpdatedEntity,
          assertEqual "updateEntity merges names when update is older" olderExpectedNames (entityNames olderUpdatedEntity),
          assertEqual "updateEntity merges labels when update is older" olderExpectedLabels (entityLabels olderUpdatedEntity)
        ]

absorbEntityTests :: Test
absorbEntityTests =
  let entity1 =
        mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        mkEntity
          (mkURI "https://example.com")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      absorbed = absorbEntity entity2 entity1
      expectedNames = Set.fromList [MkName "Test1", MkName "Test2"]
      expectedLabels = Set.fromList [MkLabel "label1", MkLabel "label2"]
   in group
        "Entity absorption"
        [ assertEqual "absorbEntity preserves original URI" (entityUri entity1) (entityUri absorbed),
          assertEqual "absorbEntity preserves original creation time" (entityCreatedAt entity1) (entityCreatedAt absorbed),
          assertBool "absorbEntity adds absorbed entity creation time to history" $ mkTime 2000 `elem` entityUpdatedAt absorbed,
          assertEqual "absorbEntity merges entity names" expectedNames (entityNames absorbed),
          assertEqual "absorbEntity merges entity labels" expectedLabels (entityLabels absorbed)
        ]

emptyCollectionTests :: Test
emptyCollectionTests =
  group
    "Empty collection"
    [ assertBool "empty collection is null" $ null empty,
      assertEqual "empty collection has zero length" 0 (length empty),
      assertBool "empty collection has empty nodes vector" $ Vector.null (collectionNodes empty),
      assertBool "empty collection has empty edges vector" $ Vector.null (collectionEdges empty),
      assertBool "empty collection has empty URI map" $ Map.null (collectionUris empty)
    ]

insertTests :: Test
insertTests =
  let entity =
        mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      (id, collection) = insert entity empty
   in group
        "Insert operations"
        [ assertEqual "insert assigns correct ID" (MkId 0) id,
          assertEqual "insert updates collection length" 1 (length collection),
          assertBool "insert makes collection non-empty" $ not (null collection),
          assertEqual "insert allows entity lookup by ID" entity (lookupEntity id collection),
          assertEqual "insert allows ID lookup by URI" (Just id) (lookupId (entityUri entity) collection)
        ]

multipleInsertTests :: Test
multipleInsertTests =
  let entity1 =
        mkEntity
          (mkURI "https://example.com/1")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        mkEntity
          (mkURI "https://example.com/2")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = insert entity2 collection1
   in group
        "Multiple insert operations"
        [ assertEqual "first insert assigns ID 0" (MkId 0) id1,
          assertEqual "second insert assigns ID 1" (MkId 1) id2,
          assertEqual "collection length after two inserts" 2 (length collection2),
          assertEqual "first entity can be retrieved" entity1 (lookupEntity id1 collection2),
          assertEqual "second entity can be retrieved" entity2 (lookupEntity id2 collection2),
          assertEqual "first URI lookup works" (Just id1) (lookupId (entityUri entity1) collection2),
          assertEqual "second URI lookup works" (Just id2) (lookupId (entityUri entity2) collection2)
        ]

upsertTests :: Test
upsertTests =
  let newEntity =
        mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      (newId, newCollection) = upsert newEntity empty

      entity1 =
        mkEntity
          (mkURI "https://example.com/existing")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        mkEntity
          (mkURI "https://example.com/existing")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = upsert entity2 collection1
      expectedEntity = absorbEntity entity2 entity1
   in group
        "Upsert operations"
        [ assertEqual "upsert of new entity assigns ID 0" (MkId 0) newId,
          assertEqual "upsert of new entity updates collection length" 1 (length newCollection),
          assertEqual "upsert of new entity allows entity lookup" newEntity (lookupEntity newId newCollection),
          assertEqual "upsert of existing entity returns same ID" id1 id2,
          assertEqual "collection length remains the same after upsert" 1 (length collection2),
          assertEqual "entity data properly merged after upsert" expectedEntity (lookupEntity id2 collection2)
        ]

edgeTests :: Test
edgeTests =
  let entity1 =
        mkEntity
          (mkURI "https://example.com/1")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        mkEntity
          (mkURI "https://example.com/2")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = insert entity2 collection1

      -- Test directed edge
      collectionWithEdge = addEdge id1 id2 collection2
      edgesFromId1 = collectionEdges collectionWithEdge ! unId id1
      edgesFromId2 = collectionEdges collectionWithEdge ! unId id2

      -- Test duplicate edge
      collectionWithDuplicateEdge = addEdge id1 id2 collectionWithEdge
      edgesFromId1AfterDuplicate = collectionEdges collectionWithDuplicateEdge ! unId id1

      -- Test bidirectional edges
      collectionWithBidirectionalEdges = addEdges id1 id2 collection2
      bidirectionalEdgesFromId1 = collectionEdges collectionWithBidirectionalEdges ! unId id1
      bidirectionalEdgesFromId2 = collectionEdges collectionWithBidirectionalEdges ! unId id2
   in group
        "Edge operations"
        [ assertBool "ids are different for edge tests" $ id1 /= id2,
          assertBool "addEdge creates forward edge" $ Vector.elem id2 edgesFromId1,
          assertBool "addEdge doesn't create backward edge" $ not (Vector.elem id1 edgesFromId2),
          assertEqual "addEdge creates exactly one edge" 1 (Vector.length edgesFromId1),
          assertEqual "duplicate addEdge doesn't create additional edges" 1 (Vector.length edgesFromId1AfterDuplicate),
          assertBool "addEdges creates forward edge" $ Vector.elem id2 bidirectionalEdgesFromId1,
          assertBool "addEdges creates backward edge" $ Vector.elem id1 bidirectionalEdgesFromId2
        ]

allTests :: Test
allTests =
  group
    "Collection tests"
    [ emptyEntityTests,
      entityTests,
      updateEntityTests,
      absorbEntityTests,
      emptyCollectionTests,
      insertTests,
      multipleInsertTests,
      upsertTests,
      edgeTests
    ]

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showSummary = showString "Summary: " . showString (if allPassed then "All tests passed!" else "Some tests failed.") . showChar '\n'
    buildString = showString (resultToString result) . showChar '\n' . showSummary
