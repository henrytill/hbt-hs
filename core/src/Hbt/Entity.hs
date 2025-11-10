{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Entity
  ( Name (..)
  , Label (..)
  , Extended (..)
  , Entity (..)
  , mkEntity
  , empty
  , update
  , absorb
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Prelude hiding (id)

newtype Name = MkName {unName :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

newtype Label = MkLabel {unLabel :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

newtype Extended = MkExtended {unExtended :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

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

instance ToJSON Entity where
  toJSON entity =
    Aeson.object
      [ "uri" .= entity.uri
      , "createdAt" .= entity.createdAt
      , "updatedAt" .= entity.updatedAt
      , "names" .= Set.toList entity.names
      , "labels" .= Set.toList entity.labels
      , "shared" .= entity.shared
      , "toRead" .= entity.toRead
      , "isFeed" .= entity.isFeed
      , "extended" .= entity.extended
      , "lastVisitedAt" .= entity.lastVisitedAt
      ]

instance FromJSON Entity where
  parseJSON = Aeson.withObject "Entity" $ \o ->
    MkEntity
      <$> o .: "uri"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> fmap Set.fromList (o .: "names")
      <*> fmap Set.fromList (o .: "labels")
      <*> o .: "shared"
      <*> o .: "toRead"
      <*> o .: "isFeed"
      <*> o .:? "extended"
      <*> o .:? "lastVisitedAt"

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
