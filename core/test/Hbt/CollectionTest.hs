{-# LANGUAGE OverloadedStrings #-}

module Hbt.CollectionTest (results) where

import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hbt.Collection
import Hbt.Entity (Entity (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Test.Dwergaz
import TestUtilities (testResults)
import Prelude hiding (length, null)

fromEither :: (Show e) => Either e a -> a
fromEither = either (error . show) id

safeURI :: Text -> URI
safeURI s = fromEither (URI.parse s)

emptyEntityTests :: Test
emptyEntityTests =
  let entity = Entity.empty
   in group
        "Entity operations on empty entity"
        [ assertEqual "emptyEntity has null URI" Entity.empty.uri entity.uri
        , assertEqual "emptyEntity has empty creation time" Entity.empty.createdAt entity.createdAt
        , assertEqual "emptyEntity has empty update history" Set.empty entity.updatedAt
        , assertEqual "emptyEntity has empty names" Set.empty entity.names
        , assertEqual "emptyEntity has empty labels" Set.empty entity.labels
        ]

entityTests :: Test
entityTests =
  let uri = safeURI "https://example.com"
      time = Time.fromSeconds 1000
      name = Just (MkName "Test Entity")
      labels = Set.fromList [MkLabel "label1", MkLabel "label2"]
      entity = Entity.mkEntity uri time name labels
      expectedNames = Set.fromList (Maybe.maybeToList name)
   in group
        "Entity operations"
        [ assertEqual "mkEntity sets correct URI" uri entity.uri
        , assertEqual "mkEntity sets correct creation time" time entity.createdAt
        , assertEqual "mkEntity sets update history with creation time" (Set.singleton time) entity.updatedAt
        , assertEqual "mkEntity sets correct names" expectedNames entity.names
        , assertEqual "mkEntity sets correct labels" labels entity.labels
        ]

updateEntityTests :: Test
updateEntityTests =
  let entity1 =
        ( Entity.mkEntity
            (safeURI "https://example.com")
            (Time.fromSeconds 1000)
            (Just (MkName "Original"))
            (Set.singleton (MkLabel "label1"))
        )
          { Entity.shared = Entity.mkShared False
          , Entity.toRead = Entity.mkToRead False
          , Entity.isFeed = Entity.mkIsFeed False
          , Entity.extended = [Entity.MkExtended "desc1"]
          }
      entity2 =
        ( Entity.mkEntity
            (safeURI "https://example.com")
            (Time.fromSeconds 2000)
            (Just (MkName "Updated"))
            (Set.singleton (MkLabel "label2"))
        )
          { Entity.shared = Entity.mkShared True
          , Entity.toRead = Entity.mkToRead True
          , Entity.isFeed = Entity.mkIsFeed True
          , Entity.extended = [Entity.MkExtended "desc2"]
          }
      absorbed = Entity.absorb entity2 entity1
      expectedNames = Set.union entity1.names entity2.names
      expectedLabels = Set.union entity1.labels entity2.labels
   in group
        "Entity absorption with monoidal behavior"
        [ assertEqual "preserves original URI" entity1.uri absorbed.uri
        , assertEqual "preserves earliest creation time" entity1.createdAt absorbed.createdAt
        , assertBool "adds absorbed creation time to history" (Time.fromSeconds 2000 `Set.member` absorbed.updatedAt)
        , assertEqual "merges entity names" expectedNames absorbed.names
        , assertEqual "merges entity labels" expectedLabels absorbed.labels
        , assertEqual "uses last-write-wins for shared" (Entity.mkShared True) absorbed.shared
        , assertEqual "uses last-write-wins for toRead" (Entity.mkToRead True) absorbed.toRead
        , assertEqual "uses logical OR for isFeed" (Entity.mkIsFeed True) absorbed.isFeed
        , assertEqual "concatenates extended" [Entity.MkExtended "desc1", Entity.MkExtended "desc2"] absorbed.extended
        ]

absorbEntityTests :: Test
absorbEntityTests =
  let entity1 =
        Entity.mkEntity
          (safeURI "https://example.com")
          (Time.fromSeconds 1000)
          (Just (MkName "Test1"))
          (Set.singleton (MkLabel "label1"))
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com")
          (Time.fromSeconds 2000)
          (Just (MkName "Test2"))
          (Set.singleton (MkLabel "label2"))
      absorbed = Entity.absorb entity2 entity1
      expectedNames = Set.union entity1.names entity2.names
      expectedLabels = Set.union entity1.labels entity2.labels
   in group
        "Entity absorption"
        [ assertEqual "absorbEntity preserves original URI" entity1.uri absorbed.uri
        , assertEqual "absorbEntity preserves original creation time" entity1.createdAt absorbed.createdAt
        , assertBool "absorbEntity adds absorbed entity creation time to history" (Time.fromSeconds 2000 `elem` absorbed.updatedAt)
        , assertEqual "absorbEntity merges entity names" expectedNames absorbed.names
        , assertEqual "absorbEntity merges entity labels" expectedLabels absorbed.labels
        ]

emptyCollectionTests :: Test
emptyCollectionTests =
  group
    "Empty collection"
    [ assertBool "empty collection is null" (null empty)
    , assertEqual "empty collection has zero length" 0 (length empty)
    ]

insertTests :: Test
insertTests =
  let entity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (Time.fromSeconds 1000)
          (Just (MkName "Test"))
          (Set.singleton (MkLabel "label"))
      (_, collection) = insert entity empty
   in group
        "Insert operations"
        [ assertEqual "insert updates collection length" 1 (length collection)
        , assertBool "insert makes collection non-empty" (not (null collection))
        , assertEqual "insert allows entity lookup by URI" (Just entity) (lookupEntity entity.uri collection)
        ]

multipleInsertTests :: Test
multipleInsertTests =
  let entity1 =
        Entity.mkEntity
          (safeURI "https://example.com/1")
          (Time.fromSeconds 1000)
          (Just (MkName "Test1"))
          (Set.singleton (MkLabel "label1"))
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com/2")
          (Time.fromSeconds 2000)
          (Just (MkName "Test2"))
          (Set.singleton (MkLabel "label2"))
      (_, collection1) = insert entity1 empty
      (_, collection2) = insert entity2 collection1
   in group
        "Multiple insert operations"
        [ assertEqual "collection length after two inserts" 2 (length collection2)
        , assertEqual "first entity can be retrieved" (Just entity1) (lookupEntity entity1.uri collection2)
        , assertEqual "second entity can be retrieved" (Just entity2) (lookupEntity entity2.uri collection2)
        ]

upsertTests :: Test
upsertTests =
  let newEntity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (Time.fromSeconds 1000)
          (Just (MkName "Test"))
          (Set.singleton (MkLabel "label"))
      (_, newCollection) = upsert newEntity empty

      entity1 =
        Entity.mkEntity
          (safeURI "https://example.com/existing")
          (Time.fromSeconds 1000)
          (Just (MkName "Test1"))
          (Set.singleton (MkLabel "label1"))
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com/existing")
          (Time.fromSeconds 2000)
          (Just (MkName "Test2"))
          (Set.singleton (MkLabel "label2"))
      (_, collection1) = insert entity1 empty
      (_, collection2) = upsert entity2 collection1
      expectedEntity = Entity.absorb entity2 entity1
   in group
        "Upsert operations"
        [ assertEqual "upsert of new entity updates collection length" 1 (length newCollection)
        , assertEqual "upsert of new entity allows entity lookup" (Just newEntity) (lookupEntity newEntity.uri newCollection)
        , assertEqual "upsert of existing entity returns same URI" entity1.uri entity2.uri
        , assertEqual "collection length remains the same after upsert" 1 (length collection2)
        , assertEqual "entity data properly merged after upsert" (Just expectedEntity) (lookupEntity entity2.uri collection2)
        ]

edgeTests :: Test
edgeTests =
  let entity1 =
        Entity.mkEntity
          (safeURI "https://example.com/1")
          (Time.fromSeconds 1000)
          (Just (MkName "Test1"))
          (Set.singleton (MkLabel "label1"))
      entity2 =
        Entity.mkEntity
          (safeURI "https://example.com/2")
          (Time.fromSeconds 2000)
          (Just (MkName "Test2"))
          (Set.singleton (MkLabel "label2"))
      (id1, collection1) = insert entity1 empty
      (id2, collection2) = insert entity2 collection1

      collectionWithEdge = addEdge id1 id2 collection2
      edgesFromId1 = edgesAt id1 collectionWithEdge
      edgesFromId2 = edgesAt id2 collectionWithEdge

      collectionWithDuplicateEdge = addEdge id1 id2 collectionWithEdge
      edgesFromId1AfterDuplicate = edgesAt id1 collectionWithDuplicateEdge

      collectionWithBidirectionalEdges = addEdges id1 id2 collection2
      bidirectionalEdgesFromId1 = edgesAt id1 collectionWithBidirectionalEdges
      bidirectionalEdgesFromId2 = edgesAt id2 collectionWithBidirectionalEdges
   in group
        "Edge operations"
        [ assertBool "ids are different for edge tests" (entity1.uri /= entity2.uri)
        , assertBool "addEdge creates forward edge" (Vector.elem id2 edgesFromId1)
        , assertBool "addEdge doesn't create backward edge" (not (Vector.elem id1 edgesFromId2))
        , assertEqual "addEdge creates exactly one edge" 1 (Vector.length edgesFromId1)
        , assertEqual "duplicate addEdge doesn't create additional edges" 1 (Vector.length edgesFromId1AfterDuplicate)
        , assertBool "addEdges creates forward edge" (Vector.elem id2 bidirectionalEdgesFromId1)
        , assertBool "addEdges creates backward edge" (Vector.elem id1 bidirectionalEdgesFromId2)
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
