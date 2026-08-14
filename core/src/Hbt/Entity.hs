{-# LANGUAGE OverloadedStrings #-}

module Hbt.Entity
  ( Name (..)
  , Label (..)
  , Shared
  , mkShared
  , getShared
  , ToRead
  , mkToRead
  , getToRead
  , IsFeed
  , mkIsFeed
  , getIsFeed
  , Extended (..)
  , LastVisitedAt (..)
  , getLastVisitedAt
  , CreatedAt
  , mkCreatedAt
  , getCreatedAt
  , lookupCreatedAt
  , Entity (..)
  , mkEntity
  , empty
  , absorb
  , fromPost
  )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe
import Data.Monoid (Last (..))
import Data.Semigroup (Min (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Hbt.Pinboard (Post (..))
import Hbt.Pinboard qualified as Pinboard

newtype Name = MkName {unName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype Label = MkLabel {unLabel :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype Shared = MkShared (Last Bool)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkShared :: Bool -> Shared
mkShared = MkShared . Last . Just

getShared :: Shared -> Maybe Bool
getShared (MkShared value) = getLast value

newtype ToRead = MkToRead (Last Bool)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkToRead :: Bool -> ToRead
mkToRead = MkToRead . Last . Just

getToRead :: ToRead -> Maybe Bool
getToRead (MkToRead value) = getLast value

newtype IsFeed = MkIsFeed (Last Bool)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkIsFeed :: Bool -> IsFeed
mkIsFeed = MkIsFeed . Last . Just

getIsFeed :: IsFeed -> Maybe Bool
getIsFeed (MkIsFeed value) = getLast value

newtype Extended = MkExtended {unExtended :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype LastVisitedAt = MkLastVisitedAt (Maybe Time)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

getLastVisitedAt :: LastVisitedAt -> Maybe Time
getLastVisitedAt (MkLastVisitedAt a) = a

instance Semigroup LastVisitedAt where
  MkLastVisitedAt a <> MkLastVisitedAt b = MkLastVisitedAt (max a b)

instance Monoid LastVisitedAt where
  mempty = MkLastVisitedAt Nothing

-- | The earliest creation time recorded for an entity.
--
-- The mirror image of 'LastVisitedAt': both wrap an optional 'Time' and keep
-- one end of the range on merge, the earliest here against the latest there,
-- with the absent value as the identity. Wrapping @Min@ in @Maybe@ gets both
-- instances from the ones they are built out of, rather than nominating a
-- sentinel time to stand in for "none recorded".
newtype CreatedAt = MkCreatedAt (Maybe (Min Time))
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid)

mkCreatedAt :: Time -> CreatedAt
mkCreatedAt = MkCreatedAt . Just . Min

-- | The recorded creation time, if there is one.
lookupCreatedAt :: CreatedAt -> Maybe Time
lookupCreatedAt (MkCreatedAt a) = fmap getMin a

-- | The creation time, defaulting to the epoch.
--
-- An entity with none recorded is one that was never given a time - only
-- 'empty' and an HTML anchor with no ADD_DATE - and the epoch is what the
-- minimum of an empty update history used to give for those.
getCreatedAt :: CreatedAt -> Time
getCreatedAt = Maybe.fromMaybe Time.epoch . lookupCreatedAt

data Entity = MkEntity
  { uri :: URI
  , createdAt :: CreatedAt
  , updatedAt :: Set Time
  -- ^ Updates, which never include 'createdAt'. The two are separate fields
  -- because the wire format distinguishes an update that merely repeats the
  -- creation time - stated outright by a LAST_MODIFIED attribute - from one
  -- that was never recorded at all.
  , names :: Set Name
  , labels :: Set Label
  , isFeed :: IsFeed
  , shared :: Shared
  , toRead :: ToRead
  , extended :: Set Extended
  , lastVisitedAt :: LastVisitedAt
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON Entity where
  toJSON entity =
    object $
      [ "uri" .= entity.uri
      , "createdAt" .= getCreatedAt entity.createdAt
      , "updatedAt" .= entity.updatedAt
      , "names" .= entity.names
      , "labels" .= entity.labels
      ]
        ++ ["isFeed" .= s | Just s <- [getIsFeed entity.isFeed]]
        ++ ["shared" .= s | Just s <- [getShared entity.shared]]
        ++ ["toRead" .= t | Just t <- [getToRead entity.toRead]]
        ++ ["extended" .= entity.extended | not (null entity.extended)]
        ++ ["lastVisitedAt" .= entity.lastVisitedAt | Maybe.isJust (getLastVisitedAt entity.lastVisitedAt)]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v -> do
    createdAt <- v .: "createdAt"
    MkEntity
      <$> v .: "uri"
      <*> pure (mkCreatedAt createdAt)
      <*> v .: "updatedAt"
      <*> v .: "names"
      <*> v .: "labels"
      <*> v .:? "isFeed" .!= mempty
      <*> v .:? "shared" .!= mempty
      <*> v .:? "toRead" .!= mempty
      <*> v .:? "extended" .!= mempty
      <*> v .:? "lastVisitedAt" .!= mempty

-- | The later of two creation times, recorded as an update.
--
-- Merging keeps the earlier creation time, so the later one would otherwise be
-- lost; it becomes an update instead. Two entities that agree on their
-- creation time record nothing, since a timestamp that merely repeats
-- createdAt carries no information - which is what bookmarks_same_timestamp
-- pins, and how the Go, OCaml and Rust implementations settled it.
supersededCreation :: CreatedAt -> CreatedAt -> Set Time
supersededCreation a b =
  case (lookupCreatedAt a, lookupCreatedAt b) of
    (Just x, Just y) | x /= y -> Set.singleton (max x y)
    _ -> Set.empty

instance Semigroup Entity where
  a <> b =
    MkEntity
      { uri = a.uri <> b.uri
      , createdAt = a.createdAt <> b.createdAt
      , updatedAt = Set.unions [a.updatedAt, b.updatedAt, supersededCreation a.createdAt b.createdAt]
      , names = a.names <> b.names
      , labels = a.labels <> b.labels
      , isFeed = a.isFeed <> b.isFeed
      , shared = a.shared <> b.shared
      , toRead = a.toRead <> b.toRead
      , extended = a.extended <> b.extended
      , lastVisitedAt = a.lastVisitedAt <> b.lastVisitedAt
      }

instance Monoid Entity where
  mempty =
    MkEntity
      { uri = mempty
      , createdAt = mempty
      , updatedAt = mempty
      , names = mempty
      , labels = mempty
      , isFeed = mempty
      , shared = mempty
      , toRead = mempty
      , extended = mempty
      , lastVisitedAt = mempty
      }

empty :: Entity
empty = mempty

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  mempty
    { uri
    , createdAt = mkCreatedAt createdAt
    , names = maybe Set.empty Set.singleton maybeName
    , labels
    }

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = existing <> other
  | otherwise = existing

nonEmpty :: Text -> Maybe Text
nonEmpty t
  | let stripped = Text.strip t
  , not $ Text.null stripped =
      Just stripped
  | otherwise = Nothing

toLabel :: Text -> Maybe Label
toLabel t = nonEmpty t <&> MkLabel

fromPost :: (HasCallStack) => Post -> IO Entity
fromPost post = do
  uri <- either throwIO pure $ URI.parse post.href
  time <- either throwIO pure $ Time.parseRFC3339 post.time
  let name = post.description >>= nonEmpty <&> MkName
  pure
    MkEntity
      { uri
      , createdAt = mkCreatedAt time
      , updatedAt = Set.empty
      , names = maybe Set.empty Set.singleton name
      , labels = Set.fromList $ Maybe.mapMaybe toLabel post.tags.unTags
      , isFeed = mkIsFeed False
      , shared = mkShared $ Pinboard.toBool post.shared
      , toRead = mkToRead $ Pinboard.toBool post.toread
      , extended = maybe Set.empty (Set.singleton . MkExtended) (post.extended >>= nonEmpty)
      , lastVisitedAt = MkLastVisitedAt Nothing
      }
