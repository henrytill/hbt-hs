{-# LANGUAGE OverloadedStrings #-}

module Hbt.Collection
  ( Id (..)
  , Error (..)
  , Collection (..)
  , SerializedNode (..)
  , SerializedCollection (..)
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
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector, elem, (!), (//))
import Data.Vector qualified as Vector
import Hbt.Collection.Entity (Entity (..), URI)
import Hbt.Collection.Entity qualified as Entity
import Prelude hiding (elem, id, length, null)

newtype Id = MkId {value :: Int}
  deriving (Show, Eq, Ord)

instance ToJSON Id where
  toJSON (MkId idValue) = toJSON idValue

instance FromJSON Id where
  parseJSON = fmap MkId . parseJSON

newtype Error = MissingEntities [URI]
  deriving (Show, Eq)

data Collection = MkCollection
  { nodes :: Vector Entity
  , edges :: Vector (Vector Id)
  , uris :: Map URI Id
  }
  deriving (Eq, Show)

data SerializedNode = SerializedNode
  { nodeId :: Id
  , entity :: Entity
  , nodeEdges :: [Id]
  }
  deriving (Show, Eq)

instance ToJSON SerializedNode where
  toJSON node =
    object
      [ "id" .= node.nodeId
      , "entity" .= node.entity
      , "edges" .= node.nodeEdges
      ]

instance FromJSON SerializedNode where
  parseJSON = withObject "SerializedNode" $ \o ->
    SerializedNode
      <$> o .: "id"
      <*> o .: "entity"
      <*> o .: "edges"

data SerializedCollection = SerializedCollection
  { version :: String
  , collectionLength :: Int
  , value :: [SerializedNode]
  }
  deriving (Show, Eq)

instance ToJSON SerializedCollection where
  toJSON coll =
    object
      [ "version" .= coll.version
      , "length" .= coll.collectionLength
      , "value" .= coll.value
      ]

instance FromJSON SerializedCollection where
  parseJSON = withObject "SerializedCollection" $ \o ->
    SerializedCollection
      <$> o .: "version"
      <*> o .: "length"
      <*> o .: "value"

empty :: Collection
empty = MkCollection Vector.empty Vector.empty Map.empty

fromEntities :: [Entity] -> Collection
fromEntities entities = foldl' (\coll entity -> snd $ upsert entity coll) empty entities

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
upsert entity collection
  | Just existingId <- lookupId entity.uri collection
  , let existing = entityAt existingId collection
  , let updated = Entity.absorb entity existing
  , updated /= existing =
      (existingId, collection {nodes = collection.nodes // [(existingId.value, updated)]})
  | Just existingId <- lookupId entity.uri collection = (existingId, collection)
  | otherwise = insert entity collection

addEdge :: Id -> Id -> Collection -> Either Error Collection
addEdge from to collection
  | validFrom && validTo =
      let fromEdges = collection.edges ! from.value
          newFromEdges = if to `elem` fromEdges then fromEdges else Vector.snoc fromEdges to
       in Right $ collection {edges = collection.edges // [(from.value, newFromEdges)]}
  | not validFrom && validTo = Left $ MissingEntities [(entityAt from collection).uri]
  | validFrom && not validTo = Left $ MissingEntities [(entityAt to collection).uri]
  | otherwise = Left $ MissingEntities [(entityAt from collection).uri, (entityAt to collection).uri]
  where
    validFrom = from.value < Vector.length collection.nodes
    validTo = to.value < Vector.length collection.nodes

addEdges :: Id -> Id -> Collection -> Either Error Collection
addEdges from to collection = addEdge from to collection >>= addEdge to from

toSerialized :: Collection -> SerializedCollection
toSerialized collection =
  SerializedCollection
    { version = "0.1.0"
    , collectionLength = Vector.length collection.nodes
    , value = Vector.toList $ Vector.imap makeNode collection.nodes
    }
  where
    makeNode i entity =
      SerializedNode
        { nodeId = MkId i
        , entity = entity
        , nodeEdges = Vector.toList $ collection.edges ! i
        }

fromSerialized :: SerializedCollection -> Collection
fromSerialized serialized =
  let entities = fmap (.entity) serialized.value
      edgesLists = fmap (.nodeEdges) serialized.value
   in MkCollection
        { nodes = Vector.fromList entities
        , edges = Vector.fromList $ fmap Vector.fromList edgesLists
        , uris = Map.fromList $ zipWith (\entity i -> (entity.uri, MkId i)) entities [0 ..]
        }

instance ToJSON Collection where
  toJSON = toJSON . toSerialized

instance FromJSON Collection where
  parseJSON = fmap fromSerialized . parseJSON
