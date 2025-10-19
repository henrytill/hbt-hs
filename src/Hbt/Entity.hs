{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Entity
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

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad.Except (liftEither, runExcept)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import URI.ByteString (URIParseError)
import URI.ByteString qualified as URI
import Prelude hiding (id)

data Error
  = InvalidURI URIParseError
  | InvalidTime Text
  deriving (Show, Eq)

instance Exception Error

newtype URI = MkURI {unURI :: URI.URIRef URI.Absolute}
  deriving (Show, Eq, Ord)

nullURI :: URI
nullURI = MkURI (URI.URI (URI.Scheme mempty) Nothing mempty (URI.Query []) Nothing)

translate :: Text -> Text
translate uriText =
  let (beforeQuery, afterQuery) = Text.breakOn "?" uriText
   in if Text.null afterQuery
        then uriText
        else beforeQuery <> Text.replace ";" "&" afterQuery

normalizeURI :: URI.URIRef URI.Absolute -> URI.URIRef URI.Absolute
normalizeURI uri
  | URI.schemeBS (URI.uriScheme uri) `elem` ["http", "https"]
  , URI.uriPath uri == mempty
  , Maybe.isJust (URI.uriAuthority uri) =
      uri {URI.uriPath = "/"}
  | otherwise = uri

mkURI :: Text -> Either Error URI
mkURI s =
  let parse text = URI.parseURI URI.laxURIParserOptions (Text.Encoding.encodeUtf8 text)
      liftedParse text = liftEither (Bifunctor.first (: []) (parse text))
      tryOriginal = liftedParse s
      tryTranslated = liftedParse (translate s)
   in case runExcept (tryOriginal <|> tryTranslated) of
        Left [] -> error "Impossible: both URI parsing attempts failed but no errors recorded"
        Left (err : _) -> Left (InvalidURI err)
        Right uri -> Right (MkURI (normalizeURI uri))

instance ToJSON URI where
  toJSON (MkURI uri) = toJSON (Text.Encoding.decodeUtf8 (URI.serializeURIRef' uri))

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
    { uri = nullURI
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
