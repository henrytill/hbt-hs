{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Collection
  ( Id (index)
  , Error (..)
  , Collection
  , new
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
  , CollectionRepr
  , toRepr
  , fromRepr
  , yamlConfig
  )
where

import Control.Exception (Exception, throw)
import Control.Monad (foldM)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Unique (Unique)
import Data.Unique qualified as Unique
import Data.Vector (Vector, elem, (!), (//))
import Data.Vector qualified as Vector
import Data.Yaml.Pretty qualified as YamlPretty
import GHC.Stack (HasCallStack)
import Hbt.Collection.Repr (CollectionRepr (..), NodeRepr (..))
import Hbt.Entity (Entity (..), fromPost)
import Hbt.Entity qualified as Entity
import Hbt.Entity.URI (URI)
import Hbt.Pinboard (Post)
import Hbt.Pinboard qualified as Pinboard
import Prelude hiding (elem, id, length, null)

data Error
  = ForeignId Id
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data Id = MkId {owner :: Unique, index :: Int}
  deriving stock (Eq)

instance Show Id where
  showsPrec _ id =
    showString "MkId {owner = "
      . shows (Unique.hashUnique id.owner)
      . showString ", index = "
      . shows id.index
      . showChar '}'

type Edges = Vector Int

data Collection = MkCollection
  { tag :: Unique
  , nodes :: Vector Entity
  , adjacency :: Vector Edges
  , uris :: Map URI Int
  }

instance Eq Collection where
  c1 == c2 = c1.nodes == c2.nodes && c1.adjacency == c2.adjacency && c1.uris == c2.uris

instance Show Collection where
  showsPrec _ c =
    showString "MkCollection {tag = "
      . shows (Unique.hashUnique c.tag)
      . showString ", nodes = "
      . shows c.nodes
      . showString ", adjacency = "
      . shows c.adjacency
      . showString ", uris = "
      . shows c.uris
      . showChar '}'

new :: IO Collection
new = do
  tag <- Unique.newUnique
  pure $ MkCollection tag Vector.empty Vector.empty Map.empty

length :: Collection -> Int
length collection = Vector.length collection.nodes

null :: Collection -> Bool
null collection = Vector.null collection.nodes

requireId :: (HasCallStack) => Collection -> Id -> Id
requireId collection id
  | id.owner == collection.tag = id
  | otherwise = throw (ForeignId id)

entityAt :: (HasCallStack) => Id -> Collection -> Entity
entityAt id collection = collection.nodes ! (requireId collection id).index

edgesAt :: (HasCallStack) => Id -> Collection -> Vector Id
edgesAt id collection = Vector.map (MkId collection.tag) (collection.adjacency ! (requireId collection id).index)

lookupId :: URI -> Collection -> Maybe Id
lookupId uri collection = fmap (MkId collection.tag) (Map.lookup uri collection.uris)

lookupEntity :: URI -> Collection -> Maybe Entity
lookupEntity uri collection = do
  id <- lookupId uri collection
  pure (entityAt id collection)

allEntities :: Collection -> Vector Entity
allEntities collection = collection.nodes

insert :: Entity -> Collection -> (Id, Collection)
insert entity collection = (newId, collection {nodes, adjacency, uris})
  where
    index = Vector.length collection.nodes
    newId = MkId collection.tag index
    nodes = Vector.snoc collection.nodes entity
    adjacency = Vector.snoc collection.adjacency Vector.empty
    uris = Map.insert entity.uri index collection.uris

upsert :: Entity -> Collection -> (Id, Collection)
upsert entity collection =
  case lookupId entity.uri collection of
    Nothing -> insert entity collection
    Just existingId ->
      let existing = entityAt existingId collection
          updated = Entity.absorb entity existing
       in if updated == existing
            then (existingId, collection)
            else (existingId, collection {nodes = collection.nodes // [(existingId.index, updated)]})

fromPosts :: [Post] -> IO Collection
fromPosts posts = do
  acc <- new
  foldM accumPosts acc (sortOn (.time) posts)
  where
    accumPosts :: Collection -> Post -> IO Collection
    accumPosts coll post = fromPost post >>= \entity -> pure (snd (upsert entity coll))

addEdge :: (HasCallStack) => Id -> Id -> Collection -> Collection
addEdge from to collection =
  let require = requireId collection
      validFrom = require from
      validTo = require to
      fromEdges = collection.adjacency ! validFrom.index
      newFromEdges
        | validTo.index `elem` fromEdges = fromEdges
        | otherwise = Vector.snoc fromEdges validTo.index
      adjacency = collection.adjacency // [(validFrom.index, newFromEdges)]
   in collection {adjacency}

addEdges :: (HasCallStack) => Id -> Id -> Collection -> Collection
addEdges from to = addEdge from to . addEdge to from

toRepr :: Collection -> CollectionRepr
toRepr collection =
  MkCollectionRepr
    { version = "0.1.0"
    , length = Vector.length collection.nodes
    , value = Vector.imap mkNodeRepr collection.nodes
    }
  where
    mkNodeRepr :: Int -> Entity -> NodeRepr
    mkNodeRepr = flip . MkNodeRepr <*> (collection.adjacency !)

fromRepr :: CollectionRepr -> IO Collection
fromRepr serialized = do
  tag <- Unique.newUnique
  pure $ MkCollection {tag, nodes, adjacency, uris}
  where
    nodes = Vector.map (.entity) serialized.value
    adjacency = Vector.map (.edges) serialized.value
    uris = Map.fromList (Vector.toList (Vector.imap (\i entity -> (entity.uri, i)) nodes))

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
