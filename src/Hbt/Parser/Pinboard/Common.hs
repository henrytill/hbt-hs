{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.Common where

import Data.Aeson (FromJSON (..), (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
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
  | otherwise = filter (\t -> not (Text.null t)) (map Text.strip (Text.splitOn " " tagStr))

instance FromJSON PinboardPost where
  parseJSON = Aeson.withObject "PinboardPost" $ \o ->
    let tags = fmap parseTagString (o .: "tags")
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
  case Format.parseTimeM True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack timeStr) :: Maybe UTCTime of
    Nothing -> Left (fromEntityErr (Entity.InvalidTime (Text.unpack timeStr)))
    Just utcTime -> Right (MkTime (POSIX.utcTimeToPOSIXSeconds utcTime))

parseTags :: [Text] -> [Label]
parseTags tagList = map (\t -> MkLabel (Text.strip t)) (filter (\t -> not (Text.null t)) tagList)

postToEntity :: (Entity.Error -> e) -> PinboardPost -> Either e Entity
postToEntity fromEntityErr post = do
  uri <- Bifunctor.first fromEntityErr (Entity.mkURI (Text.unpack post.href))
  createdAt <- parseTime fromEntityErr post.time

  let updatedAt = []
      name = if Text.null post.description then Nothing else Just (MkName post.description)
      names = maybe Set.empty Set.singleton name
      labels = Set.fromList (parseTags post.tags)
      extended = if Text.null post.extended then Nothing else Just (MkExtended post.extended)
      shared = post.shared == "yes"
      toRead = post.toread == "yes"
      isFeed = False
      lastVisitedAt = Nothing
  pure Entity.MkEntity {uri, createdAt, updatedAt, names, labels, shared, toRead, isFeed, extended, lastVisitedAt}
