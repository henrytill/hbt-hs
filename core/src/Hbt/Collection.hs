{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hbt.Collection
  ( Id (value)
  , Error (..)
  , Collection
  , empty
  , fromEntities
  , length
  , null
  , entityAt
  , edgesAt
  , lookupId
  , lookupEntity
  , allEntities
  , insert
  , upsert
  , addEdge
  , addEdges
  , toRepr
  , fromRepr
  , yamlConfig
  )
where

import Control.Exception (Exception, throw)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Vector (Vector, elem, (!), (//))
import Data.Vector qualified as Vector
import Data.Yaml.Pretty qualified as YamlPretty
import GHC.Stack (HasCallStack)
import Hbt.Collection.Id (Id (..))
import Hbt.Collection.Repr (CollectionRepr (..), NodeRepr (..))
import Hbt.Entity (Entity (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.URI (URI)
import Prelude hiding (elem, id, length, null)

newtype Error = MissingEntities [Id]
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

type Edges = Vector Id

data Collection = MkCollection
  { nodes :: Vector Entity
  , edges :: Vector Edges
  , uris :: Map URI Id
  }
  deriving stock (Eq, Show)

empty :: Collection
empty = MkCollection Vector.empty Vector.empty Map.empty

length :: Collection -> Int
length collection = Vector.length collection.nodes

null :: Collection -> Bool
null collection = Vector.null collection.nodes

entityAt :: Id -> Collection -> Entity
entityAt id collection = collection.nodes ! id.value

edgesAt :: Id -> Collection -> Edges
edgesAt id collection = collection.edges ! id.value

lookupId :: URI -> Collection -> Maybe Id
lookupId uri collection = Map.lookup uri collection.uris

lookupEntity :: URI -> Collection -> Maybe Entity
lookupEntity uri collection = do
  id <- lookupId uri collection
  pure (entityAt id collection)

allEntities :: Collection -> Vector Entity
allEntities collection = collection.nodes

insert :: Entity -> Collection -> (Id, Collection)
insert entity collection =
  let newId = MkId (Vector.length collection.nodes)
      nodes = Vector.snoc collection.nodes entity
      edges = Vector.snoc collection.edges Vector.empty
      uris = Map.insert entity.uri newId collection.uris
   in (newId, MkCollection {nodes, edges, uris})

upsert :: Entity -> Collection -> (Id, Collection)
upsert entity collection =
  case lookupId entity.uri collection of
    Nothing -> insert entity collection
    Just existingId ->
      let existing = entityAt existingId collection
          updated = Entity.absorb entity existing
       in if updated == existing
            then (existingId, collection)
            else (existingId, collection {nodes = collection.nodes // [(existingId.value, updated)]})

fromEntities :: [Entity] -> Collection
fromEntities = foldl' (\coll entity -> snd (upsert entity coll)) empty

addEdge :: (HasCallStack) => Id -> Id -> Collection -> Collection
addEdge from to collection =
  let validFrom = from.value < Vector.length collection.nodes
      validTo = to.value < Vector.length collection.nodes
   in case (validFrom, validTo) of
        (False, False) -> throw (MissingEntities [from, to])
        (False, True) -> throw (MissingEntities [from])
        (True, False) -> throw (MissingEntities [to])
        (True, True) ->
          let fromEdges = edgesAt from collection
              newFromEdges = if to `elem` fromEdges then fromEdges else Vector.snoc fromEdges to
              edges = collection.edges // [(from.value, newFromEdges)]
           in collection {Hbt.Collection.edges = edges} -- lame. are we allowing duplicate records fields or not GHC?

addEdges :: (HasCallStack) => Id -> Id -> Collection -> Collection
addEdges from to collection = addEdge from to (addEdge to from collection)

mkNodeRepr :: Collection -> Id -> Entity -> NodeRepr
mkNodeRepr collection id entity =
  let edges = edgesAt id collection
   in MkNodeRepr {id, entity, edges}

toRepr :: Collection -> CollectionRepr
toRepr collection =
  let version :: String
      version = "0.1.0"
      length = Vector.length collection.nodes
      value = Vector.imap (mkNodeRepr collection . MkId) collection.nodes
   in MkCollectionRepr {version, length, value}

fromRepr :: CollectionRepr -> Collection
fromRepr serialized =
  let entities = Vector.map (.entity) serialized.value
      nodes = entities
      edges = Vector.map (.edges) serialized.value
      uris = Map.fromList (zipWith (\entity i -> (entity.uri, MkId i)) (Vector.toList entities) [0 ..])
   in MkCollection {nodes, edges, uris}

instance ToJSON Collection where
  toJSON collection = toJSON (toRepr collection)

instance FromJSON Collection where
  parseJSON json = fmap fromRepr (parseJSON json)

-- | YAML configuration that preserves field order as expected by tests
yamlConfig :: YamlPretty.Config
yamlConfig = YamlPretty.setConfCompare fieldCompare YamlPretty.defConfig
  where
    fieldOrder :: [Text]
    fieldOrder =
      [ "version"
      , "length"
      , "value"
      , "id"
      , "entity"
      , "edges"
      , "uri"
      , "createdAt"
      , "updatedAt"
      , "names"
      , "labels"
      , "shared"
      , "toRead"
      , "isFeed"
      , "extended"
      , "lastVisitedAt"
      ]
    fieldIndex key = Maybe.fromMaybe 999 (key `elemIndex` fieldOrder)
    fieldCompare key1 key2 = compare (fieldIndex key1) (fieldIndex key2)
