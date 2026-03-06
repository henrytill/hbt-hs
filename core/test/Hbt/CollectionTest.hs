{-# LANGUAGE OverloadedStrings #-}

module Hbt.CollectionTest (results) where

import Control.Monad.State.Class (get)
import Data.Functor.Identity (runIdentity)
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
  let (isNull, len) = runIdentity $ runCollectionT $ do
        coll <- get
        pure (null coll, length coll)
   in group
        "Empty collection"
        [ assertBool "empty collection is null" isNull
        , assertEqual "empty collection has zero length" 0 len
        ]

insertTests :: Test
insertTests =
  let entity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (Time.fromSeconds 1000)
          (Just (MkName "Test"))
          (Set.singleton (MkLabel "label"))
      (len, isNonEmpty, maybeEntity) = runIdentity $ runCollectionT $ do
        _ <- insert entity
        coll <- get
        pure (length coll, not (null coll), lookupEntity entity.uri coll)
   in group
        "Insert operations"
        [ assertEqual "insert updates collection length" 1 len
        , assertBool "insert makes collection non-empty" isNonEmpty
        , assertEqual "insert allows entity lookup by URI" (Just entity) maybeEntity
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
      (len, maybeEntity1, maybeEntity2) = runIdentity $ runCollectionT $ do
        _ <- insert entity1
        _ <- insert entity2
        coll <- get
        pure (length coll, lookupEntity entity1.uri coll, lookupEntity entity2.uri coll)
   in group
        "Multiple insert operations"
        [ assertEqual "collection length after two inserts" 2 len
        , assertEqual "first entity can be retrieved" (Just entity1) maybeEntity1
        , assertEqual "second entity can be retrieved" (Just entity2) maybeEntity2
        ]

upsertTests :: Test
upsertTests =
  let newEntity =
        Entity.mkEntity
          (safeURI "https://example.com")
          (Time.fromSeconds 1000)
          (Just (MkName "Test"))
          (Set.singleton (MkLabel "label"))
      (newLen, newMaybe) = runIdentity $ runCollectionT $ do
        _ <- upsert newEntity
        coll <- get
        pure (length coll, lookupEntity newEntity.uri coll)

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
      expectedEntity = Entity.absorb entity2 entity1
      (upsertLen, upsertMaybe) = runIdentity $ runCollectionT $ do
        _ <- insert entity1
        _ <- upsert entity2
        coll <- get
        pure (length coll, lookupEntity entity2.uri coll)
   in group
        "Upsert operations"
        [ assertEqual "upsert of new entity updates collection length" 1 newLen
        , assertEqual "upsert of new entity allows entity lookup" (Just newEntity) newMaybe
        , assertEqual "upsert of existing entity returns same URI" entity1.uri entity2.uri
        , assertEqual "collection length remains the same after upsert" 1 upsertLen
        , assertEqual "entity data properly merged after upsert" (Just expectedEntity) upsertMaybe
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

      (hasFwdEdge, hasNoBkwdEdge, fwdEdgeCount) = runIdentity $ runCollectionT $ do
        id1 <- insert entity1
        id2 <- insert entity2
        addEdge id1 id2
        coll <- get
        let edges1 = edgesAt id1 coll
            edges2 = edgesAt id2 coll
        pure (Vector.elem id2 edges1, not (Vector.elem id1 edges2), Vector.length edges1)

      dupEdgeCount = runIdentity $ runCollectionT $ do
        id1 <- insert entity1
        id2 <- insert entity2
        addEdge id1 id2
        addEdge id1 id2
        coll <- get
        pure (Vector.length (edgesAt id1 coll))

      (hasBiDirFwdEdge, hasBiDirBkwdEdge) = runIdentity $ runCollectionT $ do
        id1 <- insert entity1
        id2 <- insert entity2
        addEdges id1 id2
        coll <- get
        let edges1 = edgesAt id1 coll
            edges2 = edgesAt id2 coll
        pure (Vector.elem id2 edges1, Vector.elem id1 edges2)
   in group
        "Edge operations"
        [ assertBool "ids are different for edge tests" (entity1.uri /= entity2.uri)
        , assertBool "addEdge creates forward edge" hasFwdEdge
        , assertBool "addEdge doesn't create backward edge" hasNoBkwdEdge
        , assertEqual "addEdge creates exactly one edge" 1 fwdEdgeCount
        , assertEqual "duplicate addEdge doesn't create additional edges" 1 dupEdgeCount
        , assertBool "addEdges creates forward edge" hasBiDirFwdEdge
        , assertBool "addEdges creates backward edge" hasBiDirBkwdEdge
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
