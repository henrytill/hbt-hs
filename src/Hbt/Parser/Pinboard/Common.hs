{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.Common where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hbt.Collection.Entity (Entity, Extended (..), Label (..), Name (..), Time (..))
import Hbt.Collection.Entity qualified as Entity

data Error
  = EntityInvalidURI String
  | EntityInvalidTime String
  | MissingRequiredAttribute String
  deriving (Show, Eq)

fromEntityError :: Entity.Error -> Error
fromEntityError (Entity.InvalidURI s) = EntityInvalidURI s
fromEntityError (Entity.InvalidTime s) = EntityInvalidTime s

data PinboardPost = MkPinboardPost
  { href :: Text
  , description :: Text
  , extended :: Text
  , time :: Text
  , tags :: Text
  , shared :: Text
  , toread :: Text
  }
  deriving (Show, Eq)

instance FromJSON PinboardPost where
  parseJSON = withObject "PinboardPost" $ \o ->
    MkPinboardPost
      <$> o .: "href"
      <*> o .: "description"
      <*> o .: "extended"
      <*> o .: "time"
      <*> o .: "tags"
      <*> o .: "shared"
      <*> o .:? "toread" .!= "no"

parseTime :: Text -> Either Error Time
parseTime timeStr =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack timeStr) :: Maybe UTCTime of
    Just utcTime -> Right $ MkTime (utcTimeToPOSIXSeconds utcTime)
    Nothing -> Left $ EntityInvalidTime (Text.unpack timeStr)

parseTags :: Text -> [Label]
parseTags tagStr =
  map (MkLabel . Text.strip) $
    filter (not . Text.null) $
      Text.splitOn " " tagStr

postToEntity :: PinboardPost -> Either Error Entity
postToEntity post = do
  uri <- first fromEntityError $ Entity.mkURI (Text.unpack post.href)
  createdAt <- parseTime post.time

  let name = if Text.null post.description then Nothing else Just (MkName post.description)
      labels = Set.fromList $ parseTags post.tags
      extended = if Text.null post.extended then Nothing else Just (MkExtended post.extended)
      shared = post.shared == "yes"
      toRead = post.toread == "yes"

  pure $
    (Entity.mkEntity uri createdAt name labels)
      { Entity.extended = extended
      , Entity.shared = shared
      , Entity.toRead = toRead
      , Entity.isFeed = False
      }
