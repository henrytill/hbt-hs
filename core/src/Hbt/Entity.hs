{-# LANGUAGE DeriveAnyClass #-}

module Hbt.Entity
  ( Name (..)
  , Label (..)
  , Extended (..)
  , Entity (..)
  , mkEntity
  , empty
  , update
  , absorb
  , fromPost
  )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
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
import Prelude hiding (id)

newtype Name = MkName {unName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype Label = MkLabel {unLabel :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype Extended = MkExtended {unExtended :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

data Entity = MkEntity
  { uri :: URI
  , createdAt :: Time
  , updatedAt :: [Time]
  , names :: Set Name
  , labels :: Set Label
  , shared :: Bool
  , toRead :: Bool
  , isFeed :: Bool
  , extended :: Maybe Extended
  , lastVisitedAt :: Maybe Time
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  MkEntity
    { uri
    , createdAt
    , updatedAt = []
    , names = maybe Set.empty Set.singleton maybeName
    , labels
    , shared = False
    , toRead = False
    , isFeed = False
    , extended = Nothing
    , lastVisitedAt = Nothing
    }

empty :: Entity
empty =
  MkEntity
    { uri = URI.empty
    , createdAt = Time.epoch
    , updatedAt = []
    , names = Set.empty
    , labels = Set.empty
    , shared = False
    , toRead = False
    , isFeed = False
    , extended = Nothing
    , lastVisitedAt = Nothing
    }

update :: Time -> Set Name -> Set Label -> Entity -> Entity
update updatedAt names labels entity =
  let createdAt = entity.createdAt
      updatedNames = Set.union entity.names names
      updatedLabels = Set.union entity.labels labels
   in if createdAt > updatedAt
        then
          entity
            { updatedAt = List.insert createdAt entity.updatedAt
            , createdAt = updatedAt
            , names = updatedNames
            , labels = updatedLabels
            }
        else
          entity
            { updatedAt = List.insert updatedAt entity.updatedAt
            , names = updatedNames
            , labels = updatedLabels
            }

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = update other.createdAt other.names other.labels existing
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
  uri <- either throwIO pure (URI.parse post.href)
  createdAt <- either throwIO pure (Time.parseRFC3339 post.time)
  let name = post.description >>= nonEmpty <&> MkName
  pure
    MkEntity
      { uri
      , createdAt
      , updatedAt = []
      , names = maybe Set.empty Set.singleton name
      , labels = Set.fromList (Maybe.mapMaybe toLabel post.tags.unTags)
      , shared = Pinboard.toBool post.shared
      , toRead = Pinboard.toBool post.toread
      , isFeed = False
      , extended = post.extended >>= nonEmpty <&> MkExtended
      , lastVisitedAt = Nothing
      }
