{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.CollectionTest where

import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Multimap qualified as Multimap
import Data.Set qualified as Set
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
      assertBool "empty collection has empty nodes vector" $ Map.null empty.entities,
      assertBool "empty collection has empty edges vector" $ Multimap.null empty.edges
    ]

insertTests :: Test
insertTests =
  let entity =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      collection = insert entity empty
   in group
        "Insert operations"
        [ assertEqual "insert updates collection length" 1 (length collection),
          assertBool "insert makes collection non-empty" $ not (null collection),
          assertEqual "insert allows entity lookup by URI" (Just entity) (lookupEntity entity.uri collection)
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
      collection1 = insert entity1 empty
      collection2 = insert entity2 collection1
   in group
        "Multiple insert operations"
        [ assertEqual "collection length after two inserts" 2 (length collection2),
          assertEqual "first entity can be retrieved" (Just entity1) (lookupEntity entity1.uri collection2),
          assertEqual "second entity can be retrieved" (Just entity2) (lookupEntity entity2.uri collection2)
        ]

upsertTests :: Test
upsertTests =
  let newEntity =
        Entity.mkEntity
          (mkURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      newCollection = upsert newEntity empty

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
      collection1 = insert entity1 empty
      collection2 = upsert entity2 collection1
      expectedEntity = Entity.absorb entity2 entity1
   in group
        "Upsert operations"
        [ assertEqual "upsert of new entity updates collection length" 1 (length newCollection),
          assertEqual "upsert of new entity allows entity lookup" (Just newEntity) (lookupEntity newEntity.uri newCollection),
          assertEqual "upsert of existing entity returns same URI" entity1.uri entity2.uri,
          assertEqual "collection length remains the same after upsert" 1 (length collection2),
          assertEqual "entity data properly merged after upsert" (Just expectedEntity) (lookupEntity entity2.uri collection2)
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
      collection1 = insert entity1 empty
      collection2 = insert entity2 collection1

      -- Test directed edge
      collectionWithEdge = addEdge entity1.uri entity2.uri collection2
      edgesFromURI1 = Multimap.lookup entity1.uri collectionWithEdge.edges
      edgesFromURI2 = Multimap.lookup entity1.uri collectionWithEdge.edges

      -- Test duplicate edge
      collectionWithDuplicateEdge = addEdge entity1.uri entity2.uri collectionWithEdge
      edgesFromURI1AfterDuplicate = Multimap.lookup entity1.uri collectionWithDuplicateEdge.edges

      -- Test bidirectional edges
      collectionWithBidirectionalEdges = addEdges entity1.uri entity2.uri collection2
      bidirectionalEdgesFromURI1 = Multimap.lookup entity1.uri collectionWithBidirectionalEdges.edges
      bidirectionalEdgesFromURI2 = Multimap.lookup entity2.uri collectionWithBidirectionalEdges.edges
   in group
        "Edge operations"
        [ assertBool "ids are different for edge tests" $ entity1.uri /= entity2.uri,
          assertBool "addEdge creates forward edge" $ Set.member entity2.uri edgesFromURI1,
          assertBool "addEdge doesn't create backward edge" $ not (Set.member entity1.uri edgesFromURI2),
          assertEqual "addEdge creates exactly one edge" 1 (Set.size edgesFromURI1),
          assertEqual "duplicate addEdge doesn't create additional edges" 1 (Set.size edgesFromURI1AfterDuplicate),
          assertBool "addEdges creates forward edge" $ Set.member entity2.uri bidirectionalEdgesFromURI1,
          assertBool "addEdges creates backward edge" $ Set.member entity1.uri bidirectionalEdgesFromURI2
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
    buildString = showResults . showChar '\n'
