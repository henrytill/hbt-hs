{-# LANGUAGE MultiWayIf #-}

module Hbt.Collection where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Multimap (Multimap)
import Data.Multimap qualified as Multimap
import Hbt.Collection.Entity (Entity (..))
import Hbt.Collection.Entity qualified as Entity
import Network.URI (URI)
import Text.Printf (printf)
import Prelude hiding (id, length)

data Collection = MkCollection
  { entities :: Map URI Entity
  , edges :: Multimap URI URI
  }
  deriving (Eq, Ord, Show)

instance Semigroup Collection where
  a <> b = MkCollection (a.entities <> b.entities) (a.edges <> b.edges)

empty :: Collection
empty = MkCollection Map.empty Multimap.empty

instance Monoid Collection where
  mempty = empty

length :: Collection -> Int
length = Map.size . (.entities)

null :: Collection -> Bool
null = Map.null . (.entities)

lookupEntity :: URI -> Collection -> Maybe Entity
lookupEntity uri = Map.lookup uri . (.entities)

insert :: Entity -> Collection -> Collection
insert entity collection = MkCollection entities collection.edges
  where
    entities = Map.insert entity.uri entity collection.entities

upsert :: Entity -> Collection -> Collection
upsert entity collection
  | let uri = entity.uri
  , let entities = collection.entities
  , Just existing <- Map.lookup uri entities =
      if
        | let updated = Entity.absorb entity existing
        , updated /= existing ->
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
        | not validFrom && validTo -> error $ printf "no entity for %s" (show from)
        | validFrom && not validTo -> error $ printf "no entity for %s" (show to)
        | otherwise -> error $ printf "no entities for %s and %s" (show from) (show to)

addEdges :: URI -> URI -> Collection -> Collection
addEdges from to = addEdge to from . addEdge from to
