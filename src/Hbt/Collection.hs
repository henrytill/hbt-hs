module Hbt.Collection
  ( Id (..)
  , Error (..)
  , Collection (..)
  , empty
  , length
  , null
  , lookupId
  , lookupEntity
  , entityAt
  , allEntities
  , insert
  , upsert
  , addEdge
  , addEdges
  )
where

import Control.Exception (Exception, throw)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector, elem, (!), (//))
import Data.Vector qualified as Vector
import Hbt.Collection.Entity (Entity (..), URI)
import Hbt.Collection.Entity qualified as Entity
import Prelude hiding (elem, id, length, null)

newtype Id = MkId {value :: Int}
  deriving (Show, Eq, Ord)

data Error = MissingEntities [URI]
  deriving (Show, Eq)

instance Exception Error

data Collection = MkCollection
  { nodes :: Vector Entity
  , edges :: Vector (Vector Id)
  , uris :: Map URI Id
  }
  deriving (Eq, Show)

empty :: Collection
empty = MkCollection Vector.empty Vector.empty Map.empty

length :: Collection -> Int
length = Vector.length . (.nodes)

null :: Collection -> Bool
null = Vector.null . (.nodes)

lookupId :: URI -> Collection -> Maybe Id
lookupId uri = Map.lookup uri . (.uris)

lookupEntity :: URI -> Collection -> Maybe Entity
lookupEntity uri collection = do
  id <- lookupId uri collection
  pure $ collection.nodes ! id.value

entityAt :: Id -> Collection -> Entity
entityAt id collection = collection.nodes ! id.value

allEntities :: Collection -> Vector Entity
allEntities = (.nodes)

insert :: Entity -> Collection -> (Id, Collection)
insert entity collection = (newId, newCollection)
  where
    newId = MkId (Vector.length collection.nodes)
    newCollection =
      MkCollection
        { nodes = Vector.snoc collection.nodes entity
        , edges = Vector.snoc collection.edges Vector.empty
        , uris = Map.insert entity.uri newId collection.uris
        }

upsert :: Entity -> Collection -> (Id, Collection)
upsert entity collection = case lookupId entity.uri collection of
  Just existingId ->
    let existing = entityAt existingId collection
        updated = Entity.absorb entity existing
     in if updated /= existing
          then (existingId, collection {nodes = collection.nodes // [(existingId.value, updated)]})
          else (existingId, collection)
  Nothing -> insert entity collection

addEdge :: Id -> Id -> Collection -> Collection
addEdge from to collection
  | validFrom && validTo =
      let fromEdges = collection.edges ! from.value
          newFromEdges = if to `elem` fromEdges then fromEdges else Vector.snoc fromEdges to
       in collection {edges = collection.edges // [(from.value, newFromEdges)]}
  | not validFrom && validTo = throw $ MissingEntities [(entityAt from collection).uri]
  | validFrom && not validTo = throw $ MissingEntities [(entityAt to collection).uri]
  | otherwise = throw $ MissingEntities [(entityAt from collection).uri, (entityAt to collection).uri]
  where
    validFrom = from.value < Vector.length collection.nodes
    validTo = to.value < Vector.length collection.nodes

addEdges :: Id -> Id -> Collection -> Collection
addEdges from to = addEdge from to . addEdge to from
