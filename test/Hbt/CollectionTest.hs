{-# LANGUAGE OverloadedStrings #-}

module Hbt.CollectionTest (results) where

import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hbt.Collection
import Hbt.Entity (Entity (..), Label (..), Name (..), Time (..), URI, nullURI)
import Hbt.Entity qualified as Entity
import Test.Dwergaz
import TestUtilities (testResults)
import Prelude hiding (length, null)

mkTime :: Integer -> Time
mkTime i = MkTime (fromIntegral i)

fromEither :: (Show e) => Either e a -> a
fromEither = either (error . show) id

safeURI :: Text -> URI
safeURI s = fromEither (Entity.mkURI s)

safeAddEdge :: Id -> Id -> Collection -> Collection
safeAddEdge from to collection = fromEither (addEdge from to collection)

safeAddEdges :: Id -> Id -> Collection -> Collection
safeAddEdges from to collection = fromEither (addEdges from to collection)

emptyEntityTests :: Test
emptyEntityTests =
  let entity = Entity.empty
   in group
        "Entity operations on empty entity"
        [ assertEqual "emptyEntity has null URI" nullURI entity.uri
        , assertEqual "emptyEntity has empty creation time" (mkTime 0) entity.createdAt
        , assertEqual "emptyEntity has empty update history" [] entity.updatedAt
        , assertEqual "emptyEntity has empty names" Set.empty entity.names
        , assertEqual "emptyEntity has empty labels" Set.empty entity.labels
        ]

entityTests :: Test
entityTests =
  let uri = safeURI "https://example.com"
      time = mkTime 1000
      name = Just $ MkName "Test Entity"
      labels = Set.fromList [MkLabel "label1", MkLabel "label2"]
      entity = Entity.mkEntity uri time name labels
      expectedNames = Set.fromList $ Maybe.maybeToList name
   in group
        "Entity operations"
        [ assertEqual "mkEntity sets correct URI" uri entity.uri
        , assertEqual "mkEntity sets correct creation time" time entity.createdAt
        , assertEqual "mkEntity sets empty update history" [] entity.updatedAt
        , assertEqual "mkEntity sets correct names" expectedNames entity.names
        , assertEqual "mkEntity sets correct labels" labels entity.labels
        ]

updateEntityTests :: Test
updateEntityTests =
  let baseEntity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Original")
          (Set.singleton $ MkLabel "label1")
      updateTime = mkTime 2000
      updateNames = Set.singleton $ MkName "Updated"
      updateLabels = Set.singleton $ MkLabel "label2"
      updatedEntity = Entity.update updateTime updateNames updateLabels baseEntity
      expectedNames = Set.union baseEntity.names updateNames
      expectedLabels = Set.union baseEntity.labels updateLabels
      expectedUpdates = [updateTime]

      olderBaseEntity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (mkTime 2000)
          (Just $ MkName "Original")
          (Set.singleton $ MkLabel "label1")
      olderUpdateTime = mkTime 1000
      olderUpdatedEntity = Entity.update olderUpdateTime updateNames updateLabels olderBaseEntity
      olderExpectedNames = Set.union olderBaseEntity.names updateNames
      olderExpectedLabels = Set.union olderBaseEntity.labels updateLabels
   in group
        "Entity update operations"
        [ assertEqual "updateEntity preserves creation time when update is newer" (mkTime 1000) updatedEntity.createdAt
        , assertEqual "updateEntity adds update history when update is newer" expectedUpdates updatedEntity.updatedAt
        , assertEqual "updateEntity merges names when update is newer" expectedNames updatedEntity.names
        , assertEqual "updateEntity merges labels when update is newer" expectedLabels updatedEntity.labels
        , assertEqual "updateEntity changes creation time when update is older" olderUpdateTime olderUpdatedEntity.createdAt
        , assertBool "updateEntity preserves original time in history when update is older" $ mkTime 2000 `elem` olderUpdatedEntity.updatedAt
        , assertEqual "updateEntity merges names when update is older" olderExpectedNames olderUpdatedEntity.names
        , assertEqual "updateEntity merges labels when update is older" olderExpectedLabels olderUpdatedEntity.labels
        ]

absorbEntityTests :: Test
absorbEntityTests =
  let entity1 =
        Entity.mkEntity
          (safeURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      absorbed = Entity.absorb entity2 entity1
      expectedNames = Set.union entity1.names entity2.names
      expectedLabels = Set.union entity1.labels entity2.labels
   in group
        "Entity absorption"
        [ assertEqual "absorbEntity preserves original URI" entity1.uri absorbed.uri
        , assertEqual "absorbEntity preserves original creation time" entity1.createdAt absorbed.createdAt
        , assertBool "absorbEntity adds absorbed entity creation time to history" $ mkTime 2000 `elem` absorbed.updatedAt
        , assertEqual "absorbEntity merges entity names" expectedNames absorbed.names
        , assertEqual "absorbEntity merges entity labels" expectedLabels absorbed.labels
        ]

emptyCollectionTests :: Test
emptyCollectionTests =
  group
    "Empty collection"
    [ assertBool "empty collection is null" $ null empty
    , assertEqual "empty collection has zero length" 0 $ length empty
    ]

insertTests :: Test
insertTests =
  let entity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      (_, collection) = insert entity empty
   in group
        "Insert operations"
        [ assertEqual "insert updates collection length" 1 $ length collection
        , assertBool "insert makes collection non-empty" . not $ null collection
        , assertEqual "insert allows entity lookup by URI" (Just entity) (lookupEntity entity.uri collection)
        ]

multipleInsertTests :: Test
multipleInsertTests =
  let entity1 =
        Entity.mkEntity
          (safeURI "https://example.com/1")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com/2")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (_, collection1) = insert entity1 empty
      (_, collection2) = insert entity2 collection1
   in group
        "Multiple insert operations"
        [ assertEqual "collection length after two inserts" 2 $ length collection2
        , assertEqual "first entity can be retrieved" (Just entity1) (lookupEntity entity1.uri collection2)
        , assertEqual "second entity can be retrieved" (Just entity2) (lookupEntity entity2.uri collection2)
        ]

upsertTests :: Test
upsertTests =
  let newEntity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (mkTime 1000)
          (Just $ MkName "Test")
          (Set.singleton $ MkLabel "label")
      (_, newCollection) = upsert newEntity empty

      entity1 =
        Entity.mkEntity
          (safeURI "https://example.com/existing")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com/existing")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (_, collection1) = insert entity1 empty
      (_, collection2) = upsert entity2 collection1
      expectedEntity = Entity.absorb entity2 entity1
   in group
        "Upsert operations"
        [ assertEqual "upsert of new entity updates collection length" 1 $ length newCollection
        , assertEqual "upsert of new entity allows entity lookup" (Just newEntity) (lookupEntity newEntity.uri newCollection)
        , assertEqual "upsert of existing entity returns same URI" entity1.uri entity2.uri
        , assertEqual "collection length remains the same after upsert" 1 $ length collection2
        , assertEqual "entity data properly merged after upsert" (Just expectedEntity) (lookupEntity entity2.uri collection2)
        ]

edgeTests :: Test
edgeTests =
  let entity1 =
        Entity.mkEntity
          (safeURI "https://example.com/1")
          (mkTime 1000)
          (Just $ MkName "Test1")
          (Set.singleton $ MkLabel "label1")
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com/2")
          (mkTime 2000)
          (Just $ MkName "Test2")
          (Set.singleton $ MkLabel "label2")
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = insert entity2 collection1

      collectionWithEdge = safeAddEdge id1 id2 collection2
      edgesFromId1 = edgesAt id1 collectionWithEdge
      edgesFromId2 = edgesAt id2 collectionWithEdge

      collectionWithDuplicateEdge = safeAddEdge id1 id2 collectionWithEdge
      edgesFromId1AfterDuplicate = edgesAt id1 collectionWithDuplicateEdge

      collectionWithBidirectionalEdges = safeAddEdges id1 id2 collection2
      bidirectionalEdgesFromId1 = edgesAt id1 collectionWithBidirectionalEdges
      bidirectionalEdgesFromId2 = edgesAt id2 collectionWithBidirectionalEdges
   in group
        "Edge operations"
        [ assertBool "ids are different for edge tests" $ entity1.uri /= entity2.uri
        , assertBool "addEdge creates forward edge" $ Vector.elem id2 edgesFromId1
        , assertBool "addEdge doesn't create backward edge" . not $ Vector.elem id1 edgesFromId2
        , assertEqual "addEdge creates exactly one edge" 1 $ Vector.length edgesFromId1
        , assertEqual "duplicate addEdge doesn't create additional edges" 1 $ Vector.length edgesFromId1AfterDuplicate
        , assertBool "addEdges creates forward edge" $ Vector.elem id2 bidirectionalEdgesFromId1
        , assertBool "addEdges creates backward edge" $ Vector.elem id1 bidirectionalEdgesFromId2
        ]

allTests :: Test
allTests =
  group
    "Hbt.Collection tests"
    [ emptyEntityTests
    , entityTests
    , updateEntityTests
    , absorbEntityTests
    , emptyCollectionTests
    , insertTests
    , multipleInsertTests
    , upsertTests
    , edgeTests
    ]

results :: (String, Bool)
results = testResults "Hbt.Collection" allTests
