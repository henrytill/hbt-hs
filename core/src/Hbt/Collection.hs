{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hbt.Collection
  ( Id (value)
  , Error (..)
  , Collection
  , CollectionRepr
  , empty
  , fromPosts
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
  , withCollection
  , yamlConfig
  )
where

import Control.Exception (Exception, throw)
import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (elemIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Some (Some (..))
import Data.Text (Text)
import Data.Vector (Vector, elem, (!), (//))
import Data.Vector qualified as Vector
import Data.Yaml.Pretty qualified as YamlPretty
import GHC.Stack (HasCallStack)
import Hbt.Collection.Id (Id (..))
import Hbt.Collection.Repr (CollectionRepr (..), NodeRepr (..))
import Hbt.Entity (Entity (..), fromPost)
import Hbt.Entity qualified as Entity
import Hbt.Entity.URI (URI)
import Hbt.Pinboard (Post)
import Hbt.Pinboard qualified as Pinboard
import Prelude hiding (elem, id, length, null)

newtype Error = MissingEntities [Int]
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

type role Collection nominal

type Edges s = Vector (Id s)

data Collection s = MkCollection
  { nodes :: Vector Entity
  , edges :: Vector (Edges s)
  , uris :: Map URI (Id s)
  }
  deriving stock (Eq, Show)

empty :: Collection s
empty = MkCollection Vector.empty Vector.empty Map.empty

length :: Collection s -> Int
length collection = Vector.length collection.nodes

null :: Collection s -> Bool
null collection = Vector.null collection.nodes

entityAt :: Id s -> Collection s -> Entity
entityAt id collection = collection.nodes ! id.value

edgesAt :: Id s -> Collection s -> Edges s
edgesAt id collection = collection.edges ! id.value

lookupId :: URI -> Collection s -> Maybe (Id s)
lookupId uri collection = Map.lookup uri collection.uris

lookupEntity :: URI -> Collection s -> Maybe Entity
lookupEntity uri collection = do
  id <- lookupId uri collection
  pure (entityAt id collection)

allEntities :: Collection s -> Vector Entity
allEntities collection = collection.nodes

insert :: Entity -> Collection s -> (Id s, Collection s)
insert entity collection = (newId, MkCollection {nodes, edges, uris})
  where
    newId = MkId (Vector.length collection.nodes)
    nodes = Vector.snoc collection.nodes entity
    edges = Vector.snoc collection.edges Vector.empty
    uris = Map.insert entity.uri newId collection.uris

upsert :: Entity -> Collection s -> (Id s, Collection s)
upsert entity collection =
  case lookupId entity.uri collection of
    Nothing -> insert entity collection
    Just existingId ->
      let existing = entityAt existingId collection
          updated = Entity.absorb entity existing
       in if updated == existing
            then (existingId, collection)
            else (existingId, collection {nodes = collection.nodes // [(existingId.value, updated)]})

accumPosts :: Collection s -> Post -> IO (Collection s)
accumPosts coll post = fromPost post >>= \entity -> pure (snd (upsert entity coll))

fromPosts :: [Post] -> IO (Collection s)
fromPosts posts = foldM accumPosts empty (sortOn (.time) posts)

addEdge :: (HasCallStack) => Id s -> Id s -> Collection s -> Collection s
addEdge from to collection =
  let validFrom = from.value < Vector.length collection.nodes
      validTo = to.value < Vector.length collection.nodes
   in case (validFrom, validTo) of
        (False, False) -> throw (MissingEntities [from.value, to.value])
        (False, True) -> throw (MissingEntities [from.value])
        (True, False) -> throw (MissingEntities [to.value])
        (True, True) ->
          let fromEdges = edgesAt from collection
              newFromEdges = if to `elem` fromEdges then fromEdges else Vector.snoc fromEdges to
              edges = collection.edges // [(from.value, newFromEdges)]
           in collection {Hbt.Collection.edges = edges} -- lame. are we allowing duplicate records fields or not GHC?

addEdges :: (HasCallStack) => Id s -> Id s -> Collection s -> Collection s
addEdges from to collection = addEdge from to (addEdge to from collection)

mkNodeRepr :: Collection s -> Int -> Entity -> NodeRepr
mkNodeRepr collection i entity = MkNodeRepr {id = i, entity, edges}
  where
    edges = Vector.map (.value) (edgesAt (MkId i) collection)

toRepr :: Collection s -> CollectionRepr
toRepr collection = MkCollectionRepr {version, length, value}
  where
    version = "0.1.0" :: String
    length = Vector.length collection.nodes
    value = Vector.imap (mkNodeRepr collection) collection.nodes

unsafeFromRepr :: CollectionRepr -> Collection s
unsafeFromRepr serialized = MkCollection {nodes, edges, uris}
  where
    nodes = Vector.map (.entity) serialized.value
    edges = Vector.map (Vector.map MkId . (.edges)) serialized.value
    uris = Map.fromList (zipWith (\entity i -> (entity.uri, MkId i)) (Vector.toList nodes) [0 ..])

withCollection :: CollectionRepr -> (forall s. Collection s -> a) -> a
withCollection repr k = k (unsafeFromRepr repr)

instance ToJSON (Collection s) where
  toJSON = toJSON . toRepr

instance FromJSON (Some Collection) where
  parseJSON json = do
    repr <- parseJSON json
    pure (withCollection repr Some)

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
