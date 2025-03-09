{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.CollectionTests where

import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Vector ((!))
import Data.Vector qualified as Vector
import Hbt.Collection
import Hbt.Collection.Entity (Entity (..), Label (..), Name (..), Time (..))
import Hbt.Collection.Entity qualified as Entity
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
  let entity = Entity.empty
   in group
        "Entity operations on empty entity"
        [ assertEqual "emptyEntity has null URI" URI.nullURI entity.uri,
          assertEqual "emptyEntity has empty creation time" (mkTime 0) entity.createdAt,
          assertEqual "emptyEntity has empty update history" [] entity.updatedAt,
          assertEqual "emptyEntity has empty names" Set.empty entity.names,
          assertEqual "emptyEntity has empty labels" Set.empty entity.labels
        ]

entityTests :: Test
entityTests =
  let uri = mkURI "https://example.com"
      time = mkTime 1000
      name = Just $ MkName "Test Entity"
      labels = Set.fromList [MkLabel "label1", MkLabel "label2"]
      entity = Entity.mkEntity uri time name labels
      expectedNames = Set.singleton $ MkName "Test Entity"
   in group
        "Entity operations"
        [ assertEqual "mkEntity sets correct URI" uri entity.uri,
          assertEqual "mkEntity sets correct creation time" time entity.createdAt,
          assertEqual "mkEntity sets empty update history" [] entity.updatedAt,
          assertEqual "mkEntity sets correct names" expectedNames entity.names,
          assertEqual "mkEntity sets correct labels" labels entity.labels
        ]

updateEntityTests :: Test
updateEntityTests =
  let baseEntity =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Original")
          (Set.singleton $ MkLabel "label1")
      updateTime = mkTime 2000
      updateNames = Set.singleton $ MkName "Updated"
      updateLabels = Set.singleton $ MkLabel "label2"
      updatedEntity = Entity.update updateTime updateNames updateLabels baseEntity
      expectedNames = Set.fromList [MkName "Original", MkName "Updated"]
      expectedLabels = Set.fromList [MkLabel "label1", MkLabel "label2"]
      expectedUpdates = [mkTime 2000]

      olderBaseEntity =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 2000)
          (Just $ MkName "Original")
          (Set.singleton $ MkLabel "label1")
      olderUpdateTime = mkTime 1000
      olderUpdatedEntity = Entity.update olderUpdateTime updateNames updateLabels olderBaseEntity
      olderExpectedNames = Set.fromList [MkName "Original", MkName "Updated"]
      olderExpectedLabels = Set.fromList [MkLabel "label1", MkLabel "label2"]
   in group
        "Entity update operations"
        [ assertEqual "updateEntity preserves creation time when update is newer" (mkTime 1000) updatedEntity.createdAt,
          assertEqual "updateEntity adds update history when update is newer" expectedUpdates updatedEntity.updatedAt,
          assertEqual "updateEntity merges names when update is newer" expectedNames updatedEntity.names,
          assertEqual "updateEntity merges labels when update is newer" expectedLabels updatedEntity.labels,
          assertEqual "updateEntity changes creation time when update is older" olderUpdateTime olderUpdatedEntity.createdAt,
          assertBool "updateEntity preserves original time in history when update is older" $ mkTime 2000 `elem` olderUpdatedEntity.updatedAt,
          assertEqual "updateEntity merges names when update is older" olderExpectedNames olderUpdatedEntity.names,
          assertEqual "updateEntity merges labels when update is older" olderExpectedLabels olderUpdatedEntity.labels
        ]

absorbEntityTests :: Test
absorbEntityTests =
  let entity1 =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      absorbed = Entity.absorb entity2 entity1
      expectedNames = Set.fromList [MkName "Test1", MkName "Test2"]
      expectedLabels = Set.fromList [MkLabel "label1", MkLabel "label2"]
   in group
        "Entity absorption"
        [ assertEqual "absorbEntity preserves original URI" entity1.uri absorbed.uri,
          assertEqual "absorbEntity preserves original creation time" entity1.createdAt absorbed.createdAt,
          assertBool "absorbEntity adds absorbed entity creation time to history" $ mkTime 2000 `elem` absorbed.updatedAt,
          assertEqual "absorbEntity merges entity names" expectedNames absorbed.names,
          assertEqual "absorbEntity merges entity labels" expectedLabels absorbed.labels
        ]

emptyCollectionTests :: Test
emptyCollectionTests =
  group
    "Empty collection"
    [ assertBool "empty collection is null" $ null empty,
      assertEqual "empty collection has zero length" 0 (length empty),
      assertBool "empty collection has empty nodes vector" $ Vector.null empty.nodes,
      assertBool "empty collection has empty edges vector" $ Vector.null empty.edges,
      assertBool "empty collection has empty URI map" $ Map.null empty.uris
    ]

insertTests :: Test
insertTests =
  let entity =
        Entity.mkEntity
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
          assertEqual "insert allows ID lookup by URI" (Just id) (lookupId entity.uri collection)
        ]

multipleInsertTests :: Test
multipleInsertTests =
  let entity1 =
        Entity.mkEntity
          (mkURI "https://example.com/1")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
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
          assertEqual "first URI lookup works" (Just id1) (lookupId entity1.uri collection2),
          assertEqual "second URI lookup works" (Just id2) (lookupId entity2.uri collection2)
        ]

upsertTests :: Test
upsertTests =
  let newEntity =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      (newId, newCollection) = upsert newEntity empty

      entity1 =
        Entity.mkEntity
          (mkURI "https://example.com/existing")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (mkURI "https://example.com/existing")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = upsert entity2 collection1
      expectedEntity = Entity.absorb entity2 entity1
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
        Entity.mkEntity
          (mkURI "https://example.com/1")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (mkURI "https://example.com/2")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = insert entity2 collection1

      -- Test directed edge
      collectionWithEdge = addEdge id1 id2 collection2
      edgesFromId1 = collectionWithEdge.edges ! unId id1
      edgesFromId2 = collectionWithEdge.edges ! unId id2

      -- Test duplicate edge
      collectionWithDuplicateEdge = addEdge id1 id2 collectionWithEdge
      edgesFromId1AfterDuplicate = collectionWithDuplicateEdge.edges ! unId id1

      -- Test bidirectional edges
      collectionWithBidirectionalEdges = addEdges id1 id2 collection2
      bidirectionalEdgesFromId1 = collectionWithBidirectionalEdges.edges ! unId id1
      bidirectionalEdgesFromId2 = collectionWithBidirectionalEdges.edges ! unId id2
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
    showResults = showString (resultToString result)
    showSummary = showString "Summary: " . showString (if allPassed then "All tests passed!" else "Some tests failed.")
    buildString = showResults . showChar '\n' . showSummary . showChar '\n'
