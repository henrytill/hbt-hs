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

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
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

newtype URI = MkURI {unURI :: URI.URI}
  deriving (Show, Eq, Ord)

nullURI :: URI
nullURI = MkURI URI.nullURI

normalizeURI :: URI.URI -> URI.URI
normalizeURI uri
  | URI.uriScheme uri `elem` ["http:", "https:"]
  , null (URI.uriPath uri)
  , isJust (URI.uriAuthority uri) =
      uri {URI.uriPath = "/"}
  | otherwise = uri

mkURI :: String -> Either Error URI
mkURI s =
  case parseURI s of
    Nothing -> Left $ InvalidURI s
    Just uri -> Right . MkURI $ normalizeURI uri

instance ToJSON URI where
  toJSON (MkURI uri) = toJSON $ show uri

instance FromJSON URI where
  parseJSON = withText "URI" $ \t ->
    let unpacked = Text.unpack t
     in either (fail . show) pure (mkURI unpacked)

newtype Time = MkTime {unTime :: POSIXTime}
  deriving (Show, Eq, Ord)

instance ToJSON Time where
  toJSON (MkTime posixTime) = toJSON (round posixTime :: Integer)

instance FromJSON Time where
  parseJSON = fmap (MkTime . fromInteger) . parseJSON

epoch :: Time
epoch = MkTime 0

mkTime :: String -> Either Error Time
mkTime s =
  case parseTimeM True defaultTimeLocale "%B %e, %Y" s :: Maybe UTCTime of
    Nothing -> Left $ InvalidTime s
    Just utcTime -> Right . MkTime $ utcTimeToPOSIXSeconds utcTime

newtype Name = MkName {unName :: Text}
  deriving (Show, Eq, Ord)

instance ToJSON Name where
  toJSON (MkName name) = toJSON name

instance FromJSON Name where
  parseJSON = fmap MkName . parseJSON

newtype Label = MkLabel {unLabel :: Text}
  deriving (Show, Eq, Ord)

instance ToJSON Label where
  toJSON (MkLabel label) = toJSON label

instance FromJSON Label where
  parseJSON = fmap MkLabel . parseJSON

newtype Extended = MkExtended {unExtended :: Text}
  deriving (Show, Eq, Ord)

instance ToJSON Extended where
  toJSON (MkExtended extended) = toJSON extended

instance FromJSON Extended where
  parseJSON = fmap MkExtended . parseJSON

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
    object
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
  parseJSON = withObject "Entity" $ \o ->
    MkEntity
      <$> o .: "uri"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> (Set.fromList <$> o .: "names")
      <*> (Set.fromList <$> o .: "labels")
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
