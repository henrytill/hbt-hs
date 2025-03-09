{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hbt.Collection where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as Vector
import Hbt.Collection.Entity (Entity (..))
import Hbt.Collection.Entity qualified as Entity
import Network.URI (URI)
import Prelude hiding (id, length)

newtype Id = MkId {unId :: Int}
  deriving (Show, Eq, Ord)

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
          let updated = Entity.absorb entity existing,
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
