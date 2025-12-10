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
import Data.Semigroup (Any (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Hbt.Pinboard (Post (..))
import Hbt.Pinboard qualified as Pinboard
import Prelude hiding (id)

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

newtype IsFeed = MkIsFeed Any
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkIsFeed :: Bool -> IsFeed
mkIsFeed = MkIsFeed . Any

getIsFeed :: IsFeed -> Bool
getIsFeed (MkIsFeed value) = getAny value

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

data Entity = MkEntity
  { uri :: URI
  , updatedAt :: Set Time
  , names :: Set Name
  , labels :: Set Label
  , isFeed :: IsFeed
  , shared :: Shared
  , toRead :: ToRead
  , extended :: [Extended]
  , lastVisitedAt :: LastVisitedAt
  }
  deriving stock (Eq, Ord, Show)

instance HasField "createdAt" Entity Time where
  getField entity
    | Set.null entity.updatedAt = minBound
    | otherwise = Set.findMin entity.updatedAt

instance ToJSON Entity where
  toJSON entity =
    object $
      [ "uri" .= entity.uri
      , "createdAt" .= entity.createdAt
      , "updatedAt" .= Set.delete entity.createdAt entity.updatedAt
      , "names" .= entity.names
      , "labels" .= entity.labels
      , "isFeed" .= entity.isFeed
      ]
        ++ ["shared" .= s | Just s <- [getShared entity.shared]]
        ++ ["toRead" .= t | Just t <- [getToRead entity.toRead]]
        ++ ["extended" .= entity.extended | not (null entity.extended)]
        ++ ["lastVisitedAt" .= entity.lastVisitedAt | Maybe.isJust (getLastVisitedAt entity.lastVisitedAt)]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v -> do
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    let updatedAtWithCreation = Set.insert createdAt updatedAt
    MkEntity
      <$> v .: "uri"
      <*> pure updatedAtWithCreation
      <*> v .: "names"
      <*> v .: "labels"
      <*> v .: "isFeed"
      <*> v .:? "shared" .!= mempty
      <*> v .:? "toRead" .!= mempty
      <*> v .:? "extended" .!= mempty
      <*> v .:? "lastVisitedAt" .!= mempty

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  MkEntity
    { uri
    , updatedAt = Set.singleton createdAt
    , names = maybe Set.empty Set.singleton maybeName
    , labels
    , isFeed = mkIsFeed False
    , shared = mkShared False
    , toRead = mkToRead False
    , extended = []
    , lastVisitedAt = MkLastVisitedAt Nothing
    }

instance Semigroup Entity where
  a <> b =
    MkEntity
      { uri = a.uri <> b.uri
      , updatedAt = a.updatedAt <> b.updatedAt
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

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = existing <> other
  | otherwise = existing

nonEmpty :: Text -> Maybe Text
nonEmpty t =
  let stripped = Text.strip t
   in if Text.null stripped
        then Nothing
        else Just stripped

toLabel :: Text -> Maybe Label
toLabel t = nonEmpty t <&> MkLabel

fromPost :: (HasCallStack) => Post -> IO Entity
fromPost post = do
  parsedURI <- either throwIO pure (URI.parse post.href)
  time <- either throwIO pure (Time.parseRFC3339 post.time)
  let name = post.description >>= nonEmpty <&> MkName
  pure
    MkEntity
      { uri = parsedURI
      , updatedAt = Set.singleton time
      , names = maybe Set.empty Set.singleton name
      , labels = Set.fromList (Maybe.mapMaybe toLabel post.tags.unTags)
      , isFeed = mkIsFeed False
      , shared = mkShared (Pinboard.toBool post.shared)
      , toRead = mkToRead (Pinboard.toBool post.toread)
      , extended = Maybe.maybeToList (post.extended >>= nonEmpty <&> MkExtended)
      , lastVisitedAt = MkLastVisitedAt Nothing
      }
