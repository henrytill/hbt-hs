{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hbt.Collection
  ( Id (..)
  , Error (..)
  , Collection (..)
  , empty
  , fromEntities
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
  , toSerialized
  , fromSerialized
  , yamlConfig
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, elem, (!), (//))
import Data.Vector qualified as Vector
import Data.Yaml.Pretty qualified as YamlPretty
import Hbt.Collection.Entity (Entity (..), URI)
import Hbt.Collection.Entity qualified as Entity
import Hbt.Collection.Id (Id (..))
import Hbt.Collection.Serialized (SerializedCollection (..), SerializedNode (..))
import Prelude hiding (elem, id, length, null)

newtype Error = MissingEntities [URI]
  deriving (Show, Eq)

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
fromEntities entities = foldl' (\coll entity -> snd $ upsert entity coll) empty entities

addEdge :: Id -> Id -> Collection -> Either Error Collection
addEdge from to collection =
  let validFrom = from.value < Vector.length collection.nodes
      validTo = to.value < Vector.length collection.nodes
   in case (validFrom, validTo) of
        (False, False) -> Left $ MissingEntities [(entityAt from collection).uri, (entityAt to collection).uri]
        (False, True) -> Left $ MissingEntities [(entityAt from collection).uri]
        (True, False) -> Left $ MissingEntities [(entityAt to collection).uri]
        (True, True) ->
          let fromEdges = collection.edges ! from.value
              newFromEdges = if to `elem` fromEdges then fromEdges else Vector.snoc fromEdges to
              edges = collection.edges // [(from.value, newFromEdges)]
           in Right $ collection {Hbt.Collection.edges = edges} -- lame. are we allowing duplicate records fields or not GHC?

addEdges :: Id -> Id -> Collection -> Either Error Collection
addEdges from to collection = addEdge from to collection >>= addEdge to from

mkSerializedNode :: Vector (Vector Id) -> Id -> Entity -> SerializedNode
mkSerializedNode edges id entity = SerializedNode {id, entity, edges = edges ! id.value}

toSerialized :: Collection -> SerializedCollection
toSerialized collection =
  let version = "0.1.0"
      length = Vector.length collection.nodes
      value = Vector.imap (mkSerializedNode collection.edges . MkId) collection.nodes
   in SerializedCollection {version, length, value}

fromSerialized :: SerializedCollection -> Collection
fromSerialized serialized =
  let entities = fmap (.entity) serialized.value
      nodes = entities
      edges = fmap (.edges) serialized.value
      uris = Map.fromList $ zipWith (\entity i -> (entity.uri, MkId i)) (Vector.toList entities) [0 ..]
   in MkCollection {nodes, edges, uris}

instance ToJSON Collection where
  toJSON = toJSON . toSerialized

instance FromJSON Collection where
  parseJSON = fmap fromSerialized . parseJSON

-- | YAML configuration that preserves field order as expected by tests
yamlConfig :: YamlPretty.Config
yamlConfig = YamlPretty.setConfCompare fieldCompare YamlPretty.defConfig
  where
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
    fieldIndex key = fromMaybe 999 (key `elemIndex` fieldOrder)
    fieldCompare key1 key2 = compare (fieldIndex key1) (fieldIndex key2)
