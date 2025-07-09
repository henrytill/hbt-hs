module Hbt.Collection.Entity
  ( Error (..)
  , mkURI
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
import Network.URI (URI)
import Network.URI qualified as URI
import Prelude hiding (id)

data Error
  = InvalidURI String
  | InvalidTime String
  deriving (Show, Eq)

instance Exception Error

mkURI :: String -> URI
mkURI s = Maybe.fromMaybe (throw $ InvalidURI s) (URI.parseURI s)

newtype Time = MkTime {unTime :: POSIXTime}
  deriving (Show, Eq, Ord)

epoch :: Time
epoch = MkTime 0

parsePOSIXTime :: String -> POSIXTime
parsePOSIXTime s = case parseTimeM True defaultTimeLocale "%B %e, %Y" s :: Maybe UTCTime of
  Nothing -> throw $ InvalidTime s
  Just utcTime -> utcTimeToPOSIXSeconds utcTime

mkTime :: String -> Time
mkTime s = MkTime $ parsePOSIXTime s

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
  , extended :: Maybe Extended
  , shared :: Bool
  , toRead :: Bool
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
    , extended = Nothing
    , shared = False
    , toRead = False
    }

empty :: Entity
empty =
  MkEntity
    { uri = URI.nullURI
    , createdAt = epoch
    , updatedAt = []
    , names = Set.empty
    , labels = Set.empty
    , extended = Nothing
    , shared = False
    , toRead = False
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
