{-# LANGUAGE OverloadedRecordDot #-}

module Hbt.Collection.Entity
  ( Time (..),
    Name (..),
    Label (..),
    Extended (..),
    Entity (..),
    mkEntity,
    empty,
    update,
    absorb,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.URI (URI)
import Network.URI qualified as URI
import Prelude hiding (id)

newtype Time = MkTime {unTime :: POSIXTime}
  deriving (Show, Eq, Ord)

emptyTime :: Time
emptyTime = MkTime 0

newtype Name = MkName {unName :: Text}
  deriving (Show, Eq, Ord)

newtype Label = MkLabel {unLabel :: Text}
  deriving (Show, Eq, Ord)

newtype Extended = MkExtended {unExtended :: Text}
  deriving (Show, Eq, Ord)

data Entity = MkEntity
  { uri :: URI,
    createdAt :: Time,
    updatedAt :: [Time],
    names :: Set Name,
    labels :: Set Label,
    extended :: Maybe Extended,
    shared :: Bool,
    toRead :: Bool
  }
  deriving (Show, Eq, Ord)

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  MkEntity
    { uri,
      createdAt,
      updatedAt = [],
      names = maybe Set.empty Set.singleton maybeName,
      labels,
      extended = Nothing,
      shared = False,
      toRead = False
    }

empty :: Entity
empty =
  MkEntity
    { uri = URI.nullURI,
      createdAt = emptyTime,
      updatedAt = [],
      names = Set.empty,
      labels = Set.empty,
      extended = Nothing,
      shared = False,
      toRead = False
    }

update :: Time -> Set Name -> Set Label -> Entity -> Entity
update updatedAt names labels entity
  | let createdAt = entity.createdAt,
    createdAt > updatedAt =
      entity
        { updatedAt = createdAt : entity.updatedAt,
          createdAt = updatedAt,
          names = updatedNames,
          labels = updatedLabels
        }
  | otherwise =
      entity
        { updatedAt = updatedAt : entity.updatedAt,
          names = updatedNames,
          labels = updatedLabels
        }
  where
    updatedNames = Set.union entity.names names
    updatedLabels = Set.union entity.labels labels

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = update other.createdAt other.names other.labels existing
  | otherwise = existing
