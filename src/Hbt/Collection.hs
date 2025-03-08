{-# LANGUAGE MultiWayIf #-}

module Hbt.Collection where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as Vector
import Network.URI (URI)
import Network.URI qualified as URI
import Prelude hiding (id, length)

newtype Id = MkId {unId :: Int}
  deriving (Show, Eq, Ord)

newtype Time = MkTime {unTime :: POSIXTime}
  deriving (Show, Eq, Ord)

emptyTime :: Time
emptyTime = MkTime 0

newtype Name = MkName {unName :: Text}
  deriving (Show, Eq, Ord)

newtype Label = MkLabel {unLabel :: Text}
  deriving (Show, Eq, Ord)

newtype Extended = MkExtended {unExtended :: Text}
  deriving (Show, Eq, Ord)

data Entity = MkEntity
  { entityUri :: URI,
    entityCreatedAt :: Time,
    entityUpdatedAt :: [Time],
    entityNames :: Set Name,
    entityLabels :: Set Label,
    entityExtended :: Maybe Extended,
    entityShared :: Bool,
    entityToRead :: Bool
  }
  deriving (Show, Eq, Ord)

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  MkEntity
    { entityUri = uri,
      entityCreatedAt = createdAt,
      entityUpdatedAt = [],
      entityNames = maybe Set.empty Set.singleton maybeName,
      entityLabels = labels,
      entityExtended = Nothing,
      entityShared = False,
      entityToRead = False
    }

emptyEntity :: Entity
emptyEntity =
  MkEntity
    { entityUri = URI.nullURI,
      entityCreatedAt = emptyTime,
      entityUpdatedAt = [],
      entityNames = Set.empty,
      entityLabels = Set.empty,
      entityExtended = Nothing,
      entityShared = False,
      entityToRead = False
    }

updateEntity :: Time -> Set Name -> Set Label -> Entity -> Entity
updateEntity updatedAt names labels entity
  | let createdAt = entityCreatedAt entity,
    createdAt > updatedAt =
      entity
        { entityUpdatedAt = createdAt : entityUpdatedAt entity,
          entityCreatedAt = updatedAt,
          entityNames = updatedNames,
          entityLabels = updatedLabels
        }
  | otherwise =
      entity
        { entityUpdatedAt = updatedAt : entityUpdatedAt entity,
          entityNames = updatedNames,
          entityLabels = updatedLabels
        }
  where
    updatedNames = Set.union (entityNames entity) names
    updatedLabels = Set.union (entityLabels entity) labels

absorbEntity :: Entity -> Entity -> Entity
absorbEntity other existing
  | other /= existing =
      updateEntity
        (entityCreatedAt other)
        (entityNames other)
        (entityLabels other)
        existing
  | otherwise = existing

type Edges = Vector Id

data Collection = MkCollection
  { collectionNodes :: Vector Entity,
    collectionEdges :: Vector Edges,
    collectionUris :: Map URI Id
  }

empty :: Collection
empty =
  MkCollection
    { collectionNodes = Vector.empty,
      collectionEdges = Vector.empty,
      collectionUris = Map.empty
    }

length :: Collection -> Int
length = Vector.length . collectionNodes

null :: Collection -> Bool
null = Vector.null . collectionNodes

lookupId :: URI -> Collection -> Maybe Id
lookupId uri = Map.lookup uri . collectionUris

lookupEntity :: Id -> Collection -> Entity
lookupEntity (MkId i) collection = collectionNodes collection ! i

insert :: Entity -> Collection -> (Id, Collection)
insert entity collection = (id, MkCollection updatedNodes updatedEdges updatedUris)
  where
    id = MkId $ length collection
    updatedNodes = Vector.snoc (collectionNodes collection) entity
    updatedEdges = Vector.snoc (collectionEdges collection) Vector.empty
    updatedUris = Map.insert (entityUri entity) id (collectionUris collection)

upsert :: Entity -> Collection -> (Id, Collection)
upsert entity collection
  | let uri = entityUri entity,
    Just id@(MkId i) <- lookupId uri collection =
      if
        | let nodes = collectionNodes collection,
          let updatedEntity = absorbEntity entity (nodes ! i),
          updatedEntity /= entity ->
            (id, collection {collectionNodes = nodes // [(i, updatedEntity)]})
        | otherwise ->
            (id, collection)
  | otherwise = insert entity collection

addEdge :: Id -> Id -> Collection -> Collection
addEdge (MkId i) to collection
  | let edges = collectionEdges collection,
    let fromEdges = edges ! i,
    not $ Vector.elem to fromEdges =
      collection {collectionEdges = edges // [(i, Vector.snoc fromEdges to)]}
  | otherwise = collection

addEdges :: Id -> Id -> Collection -> Collection
addEdges from to = addEdge to from . addEdge from to
