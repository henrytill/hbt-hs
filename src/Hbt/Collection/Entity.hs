{-# LANGUAGE OverloadedStrings #-}

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

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import Network.URI qualified as URI
import Prelude hiding (id)

data Error
  = InvalidURI Text
  | InvalidTime Text
  deriving (Show, Eq)

newtype URI = MkURI {unURI :: URI.URI}
  deriving (Show, Eq, Ord)

nullURI :: URI
nullURI = MkURI URI.nullURI

normalizeURI :: URI.URI -> URI.URI
normalizeURI uri
  | URI.uriScheme uri `elem` ["http:", "https:"]
  , null (URI.uriPath uri)
  , Maybe.isJust (URI.uriAuthority uri) =
      uri {URI.uriPath = "/"}
  | otherwise = uri

mkURI :: Text -> Either Error URI
mkURI s =
  case URI.parseURI (Text.unpack s) of
    Nothing -> Left (InvalidURI s)
    Just uri -> Right (MkURI (normalizeURI uri))

instance ToJSON URI where
  toJSON (MkURI uri) = toJSON (show uri)

instance FromJSON URI where
  parseJSON = Aeson.withText "URI" $ \t ->
    either (fail . show) pure (mkURI t)

newtype Time = MkTime {unTime :: POSIXTime}
  deriving (Show, Eq, Ord)

instance ToJSON Time where
  toJSON (MkTime posixTime) = toJSON @Integer (round posixTime)

instance FromJSON Time where
  parseJSON json = fmap (MkTime . fromInteger) (parseJSON json)

epoch :: Time
epoch = MkTime 0

mkTime :: Text -> Either Error Time
mkTime s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%B %e, %Y" (Text.unpack s) of
    Nothing -> Left (InvalidTime s)
    Just utcTime -> Right (MkTime (POSIX.utcTimeToPOSIXSeconds utcTime))

newtype Name = MkName {unName :: Text}
  deriving (Show, Eq, Ord)

instance ToJSON Name where
  toJSON (MkName name) = toJSON name

instance FromJSON Name where
  parseJSON json = fmap MkName (parseJSON json)

newtype Label = MkLabel {unLabel :: Text}
  deriving (Show, Eq, Ord)

instance ToJSON Label where
  toJSON (MkLabel label) = toJSON label

instance FromJSON Label where
  parseJSON json = fmap MkLabel (parseJSON json)

newtype Extended = MkExtended {unExtended :: Text}
  deriving (Show, Eq, Ord)

instance ToJSON Extended where
  toJSON (MkExtended extended) = toJSON extended

instance FromJSON Extended where
  parseJSON json = fmap MkExtended (parseJSON json)

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

insertSorted :: Time -> [Time] -> [Time]
insertSorted t [] = [t]
insertSorted t (x : xs)
  | t <= x = t : x : xs
  | otherwise = x : insertSorted t xs

update :: Time -> Set Name -> Set Label -> Entity -> Entity
update updatedAt names labels entity =
  let createdAt = entity.createdAt
      updatedNames = Set.union entity.names names
      updatedLabels = Set.union entity.labels labels
   in if createdAt > updatedAt
        then
          entity
            { updatedAt = insertSorted createdAt entity.updatedAt
            , createdAt = updatedAt
            , names = updatedNames
            , labels = updatedLabels
            }
        else
          entity
            { updatedAt = insertSorted updatedAt entity.updatedAt
            , names = updatedNames
            , labels = updatedLabels
            }

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = update other.createdAt other.names other.labels existing
  | otherwise = existing
