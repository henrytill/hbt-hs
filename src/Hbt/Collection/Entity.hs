module Hbt.Collection.Entity
  ( Error (..)
  , URI (..)
  , mkURI
  , nullURI
  , Time (..)
  , mkTime
  , Name (..)
  , Label (..)
  , Extended (..)
  , Entity (..)
  , mkEntity
  , empty
  , update
  , absorb
  )
where

import Control.Exception (Exception, throw)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.URI (parseURI)
import Network.URI qualified as URI
import Prelude hiding (id)

data Error
  = InvalidURI String
  | InvalidTime String
  deriving (Show, Eq)

instance Exception Error

newtype URI = MkURI {unURI :: URI.URI}
  deriving (Show, Eq, Ord)

mkURI :: String -> URI
mkURI s = MkURI $ Maybe.fromMaybe (throw $ InvalidURI s) (parseURI s)

nullURI :: URI
nullURI = MkURI URI.nullURI

newtype Time = MkTime {unTime :: POSIXTime}
  deriving (Show, Eq, Ord)

epoch :: Time
epoch = MkTime 0

mkTime :: String -> Time
mkTime s = case parseTimeM True defaultTimeLocale "%B %e, %Y" s :: Maybe UTCTime of
  Nothing -> throw $ InvalidTime s
  Just utcTime -> MkTime $ utcTimeToPOSIXSeconds utcTime

newtype Name = MkName {unName :: Text}
  deriving (Show, Eq, Ord)

newtype Label = MkLabel {unLabel :: Text}
  deriving (Show, Eq, Ord)

newtype Extended = MkExtended {unExtended :: Text}
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

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
    { uri = MkURI URI.nullURI
    , createdAt = epoch
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
update updatedAt names labels entity
  | let createdAt = entity.createdAt
  , createdAt > updatedAt =
      entity
        { updatedAt = createdAt : entity.updatedAt
        , createdAt = updatedAt
        , names = updatedNames
        , labels = updatedLabels
        }
  | otherwise =
      entity
        { updatedAt = updatedAt : entity.updatedAt
        , names = updatedNames
        , labels = updatedLabels
        }
  where
    updatedNames = Set.union entity.names names
    updatedLabels = Set.union entity.labels labels

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = update other.createdAt other.names other.labels existing
  | otherwise = existing
