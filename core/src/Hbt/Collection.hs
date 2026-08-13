{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Collection
  ( Error (..)
  , Id
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

import Control.Exception (Exception, throw, throwIO)
import Control.Monad (foldM)
import Control.Monad qualified as Monad
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

data Id = MkId {owner :: Unique, index :: Int}
  deriving stock (Eq)

instance Show Id where
  showsPrec _ id =
    showString "MkId {owner = "
      . shows (Unique.hashUnique id.owner)
      . showString ", index = "
      . shows id.index
      . showChar '}'

data Error
  = ForeignId Id
  | -- | The declared length disagreed with the number of nodes: declared, actual.
    LengthMismatch Int Int
  | -- | A node's id was not its position once the nodes were sorted by id.
    -- Because they are sorted first, this one comparison rejects gaps,
    -- negative and out-of-range ids, and duplicates alike: expected, found.
    UnexpectedId Int Int
  | -- | An edge pointed outside the collection: source node, edge target.
    EdgeOutOfBounds Int Int
  | -- | Two nodes shared a URI, which would leave the first unreachable
    -- through the uri index: node, uri.
    DuplicateURI Int URI
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

type Edges = Vector Int

data Collection = MkCollection
  { tag :: Unique
  , nodes :: Vector Entity
  , edges :: Vector Edges
  , uris :: Map URI Int
  }

instance Eq Collection where
  c1 == c2 = c1.nodes == c2.nodes && c1.edges == c2.edges && c1.uris == c2.uris

instance Show Collection where
  showsPrec _ c =
    showString "MkCollection {tag = "
      . shows (Unique.hashUnique c.tag)
      . showString ", nodes = "
      . shows c.nodes
      . showString ", edges = "
      . shows c.edges
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
edgesAt id collection = Vector.map (MkId collection.tag) (collection.edges ! (requireId collection id).index)

lookupId :: URI -> Collection -> Maybe Id
lookupId uri collection = fmap (MkId collection.tag) (Map.lookup uri collection.uris)

lookupEntity :: URI -> Collection -> Maybe Entity
lookupEntity uri collection = do
  id <- lookupId uri collection
  pure $ entityAt id collection

allEntities :: Collection -> Vector Entity
allEntities collection = collection.nodes

insert :: Entity -> Collection -> (Id, Collection)
insert entity collection = (newId, collection {nodes, edges, uris})
  where
    index = Vector.length collection.nodes
    newId = MkId collection.tag index
    nodes = Vector.snoc collection.nodes entity
    edges = Vector.snoc collection.edges Vector.empty
    uris = Map.insert entity.uri index collection.uris

upsert :: Entity -> Collection -> (Id, Collection)
upsert entity collection =
  case lookupId entity.uri collection of
    Nothing -> insert entity collection
    Just existingId
      | updated == existing -> (existingId, collection)
      | otherwise -> (existingId, collection {nodes})
      where
        existing = entityAt existingId collection
        updated = Entity.absorb entity existing
        nodes = collection.nodes // [(existingId.index, updated)]

fromPosts :: [Post] -> IO Collection
fromPosts posts = do
  coll <- new
  foldM accumPosts coll (sortOn (.time) posts)
  where
    accumPosts :: Collection -> Post -> IO Collection
    accumPosts acc post = fromPost post >>= pure . snd . flip upsert acc

addEdge :: (HasCallStack) => Id -> Id -> Collection -> Collection
addEdge from to collection
  | validTo.index `elem` fromEdges = collection
  | otherwise = collection {Hbt.Collection.edges}
  where
    validFrom = requireId collection from
    validTo = requireId collection to
    fromEdges = collection.edges ! validFrom.index
    edges = collection.edges // [(validFrom.index, Vector.snoc fromEdges validTo.index)]

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
    mkNodeRepr = flip . MkNodeRepr <*> (collection.edges !)

-- | Rebuild a collection from its serialized form, rejecting data that would
-- leave it in a state the rest of the module assumes cannot happen.
--
-- Nothing downstream re-checks any of this, so whatever gets past here is
-- trusted: an out-of-range edge index later hands out an 'Id' whose lookup
-- fails on a raw vector bounds error, and a duplicated URI leaves an entity
-- that cannot be found by its own URI.
--
-- A node with no URI needs no check here: 'Hbt.Entity.URI.parse' rejects the
-- empty string, so an entity without one never deserializes in the first
-- place.
fromRepr :: CollectionRepr -> IO Collection
fromRepr serialized = do
  Monad.when (serialized.length /= count) $
    throwIO (LengthMismatch serialized.length count)
  Vector.imapM_ checkId sorted
  Vector.imapM_ checkEdges edges
  uris <- Vector.ifoldM' insertURI Map.empty nodes
  tag <- Unique.newUnique
  pure $ MkCollection {tag, nodes, edges, uris}
  where
    -- Sorting first means a file that lists its nodes out of order is still
    -- read correctly, since edges refer to ids rather than to positions.
    sorted = Vector.fromList (sortOn (.id) (Vector.toList serialized.value))
    count = Vector.length sorted
    nodes = Vector.map (.entity) sorted
    edges = Vector.map (.edges) sorted

    checkId index node =
      Monad.when (node.id /= index) $ throwIO (UnexpectedId index node.id)

    checkEdges index =
      Vector.mapM_ $ \target ->
        Monad.when (target < 0 || target >= count) $
          throwIO (EdgeOutOfBounds index target)

    insertURI acc index entity
      | Map.member entity.uri acc = throwIO (DuplicateURI index entity.uri)
      | otherwise = pure (Map.insert entity.uri index acc)

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
