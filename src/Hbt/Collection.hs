{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hbt.Collection where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Multimap (Multimap)
import Data.Multimap qualified as Multimap
import Hbt.Collection.Entity (Entity (..))
import Hbt.Collection.Entity qualified as Entity
import Network.URI (URI)
import Prelude hiding (id, length)

data Collection = MkCollection
  { entities :: Map URI Entity,
    edges :: Multimap URI URI
  }

instance Semigroup Collection where
  a <> b = MkCollection (a.entities <> b.entities) (a.edges <> b.edges)

instance Monoid Collection where
  mempty = empty

empty :: Collection
empty =
  MkCollection
    { entities = Map.empty,
      edges = Multimap.empty
    }

length :: Collection -> Int
length = Map.size . entities

null :: Collection -> Bool
null = Map.null . entities

lookupEntity :: URI -> Collection -> Maybe Entity
lookupEntity uri collection = Map.lookup uri collection.entities

insert :: Entity -> Collection -> Collection
insert entity collection = MkCollection entities edges
  where
    entities = Map.insert entity.uri entity collection.entities
    edges = Multimap.empty

upsert :: Entity -> Collection -> Collection
upsert entity collection
  | let uri = entity.uri,
    let entities = collection.entities,
    Just existing <- Map.lookup uri entities =
      if
        | let updated = Entity.absorb entity existing,
          updated /= existing ->
            collection {entities = Map.insert uri updated entities}
        | otherwise ->
            collection
  | otherwise = insert entity collection

addEdge :: URI -> URI -> Collection -> Collection
addEdge from to collection =
  let entities = collection.entities
      validFrom = Map.member from entities
      validTo = Map.member to entities
   in if
        | validFrom && validTo -> collection {edges = Multimap.insert from to collection.edges}
        | not validFrom && validTo -> error $ "no entity for " ++ show from
        | validFrom && not validTo -> error $ "no entity for " ++ show to
        | otherwise -> error $ "no entities for " ++ show from ++ " and " ++ show to

addEdges :: URI -> URI -> Collection -> Collection
addEdges from to = addEdge to from . addEdge from to
