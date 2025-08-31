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

data PinboardPost = MkPinboardPost
  { href :: Text
  , description :: Text
  , extended :: Text
  , time :: Text
  , tags :: [Text]
  , shared :: Text
  , toread :: Text
  }
  deriving (Show, Eq)

parseTagString :: Text -> [Text]
parseTagString tagStr
  | Text.null tagStr = []
  | otherwise = filter (not . Text.null) $ map Text.strip $ Text.splitOn " " tagStr

instance FromJSON PinboardPost where
  parseJSON = withObject "PinboardPost" $ \o ->
    let tags = parseTagString <$> o .: "tags"
     in MkPinboardPost
          <$> o .: "href"
          <*> o .: "description"
          <*> o .: "extended"
          <*> o .: "time"
          <*> tags
          <*> o .: "shared"
          <*> o .:? "toread" .!= "no"

parseTime :: (Entity.Error -> e) -> Text -> Either e Time
parseTime fromEntityErr timeStr =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack timeStr) :: Maybe UTCTime of
    Nothing -> Left $ fromEntityErr (Entity.InvalidTime (Text.unpack timeStr))
    Just utcTime -> Right $ MkTime (utcTimeToPOSIXSeconds utcTime)

parseTags :: [Text] -> [Label]
parseTags tagList = map (MkLabel . Text.strip) $ filter (not . Text.null) tagList

postToEntity :: (Entity.Error -> e) -> PinboardPost -> Either e Entity
postToEntity fromEntityErr post = do
  uri <- first fromEntityErr $ Entity.mkURI (Text.unpack post.href)
  createdAt <- parseTime fromEntityErr post.time

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
