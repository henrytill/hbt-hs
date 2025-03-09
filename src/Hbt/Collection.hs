{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
  { uri :: URI,
    createdAt :: Time,
    updatedAt :: [Time],
    names :: Set Name,
    labels :: Set Label,
    extended :: Maybe Extended,
    shared :: Bool,
    toRead :: Bool
  }
  deriving (Show, Eq, Ord)

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  MkEntity
    { uri,
      createdAt,
      updatedAt = [],
      names = maybe Set.empty Set.singleton maybeName,
      labels,
      extended = Nothing,
      shared = False,
      toRead = False
    }

emptyEntity :: Entity
emptyEntity =
  MkEntity
    { uri = URI.nullURI,
      createdAt = emptyTime,
      updatedAt = [],
      names = Set.empty,
      labels = Set.empty,
      extended = Nothing,
      shared = False,
      toRead = False
    }

updateEntity :: Time -> Set Name -> Set Label -> Entity -> Entity
updateEntity updatedAt names labels entity
  | let createdAt = entity.createdAt,
    createdAt > updatedAt =
      entity
        { updatedAt = createdAt : entity.updatedAt,
          createdAt = updatedAt,
          names = updatedNames,
          labels = updatedLabels
        }
  | otherwise =
      entity
        { updatedAt = updatedAt : entity.updatedAt,
          names = updatedNames,
          labels = updatedLabels
        }
  where
    updatedNames = Set.union entity.names names
    updatedLabels = Set.union entity.labels labels

absorbEntity :: Entity -> Entity -> Entity
absorbEntity other existing
  | other /= existing = updateEntity other.createdAt other.names other.labels existing
  | otherwise = existing

type Edges = Vector Id

data Collection = MkCollection
  { nodes :: Vector Entity,
    edges :: Vector Edges,
    uris :: Map URI Id
  }

empty :: Collection
empty =
  MkCollection
    { nodes = Vector.empty,
      edges = Vector.empty,
      uris = Map.empty
    }

length :: Collection -> Int
length = Vector.length . nodes

null :: Collection -> Bool
null = Vector.null . nodes

lookupId :: URI -> Collection -> Maybe Id
lookupId uri = Map.lookup uri . uris

lookupEntity :: Id -> Collection -> Entity
lookupEntity (MkId i) collection = collection.nodes ! i

insert :: Entity -> Collection -> (Id, Collection)
insert entity collection = (id, MkCollection updatedNodes updatedEdges updatedUris)
  where
    id = MkId $ length collection
    updatedNodes = Vector.snoc collection.nodes entity
    updatedEdges = Vector.snoc collection.edges Vector.empty
    updatedUris = Map.insert entity.uri id collection.uris

upsert :: Entity -> Collection -> (Id, Collection)
upsert entity collection
  | Just id@(MkId i) <- lookupId entity.uri collection =
      if
        | let nodes = collection.nodes,
          let existing = nodes ! i,
          let updated = absorbEntity entity existing,
          updated /= existing ->
            (id, collection {nodes = nodes // [(i, updated)]})
        | otherwise ->
            (id, collection)
  | otherwise = insert entity collection

addEdge :: Id -> Id -> Collection -> Collection
addEdge (MkId i) to collection
  | let edges = collection.edges,
    let entityEdges = edges ! i,
    not $ Vector.elem to entityEdges =
      collection {edges = edges // [(i, Vector.snoc entityEdges to)]}
  | otherwise = collection

addEdges :: Id -> Id -> Collection -> Collection
addEdges from to = addEdge to from . addEdge from to
