{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Parser.Pinboard.Common
  ( PinboardBool (..)
  , pattern PinboardTrue
  , pattern PinboardFalse
  , PinboardPost (..)
  , emptyPinboardPost
  , epochTimeText
  , parseTagString
  , parseTags
  , parseTime
  , postToEntity
  )
where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import GHC.Generics (Generic)
import Hbt.Collection.Entity (Entity, Extended (..), Label (..), Name (..), Time (..))
import Hbt.Collection.Entity qualified as Entity
import Hbt.Parser.Common (IsNull (..), pattern Null)

newtype PinboardBool = MkPinboardBool Text
  deriving (Show, Eq, Generic)

pattern PinboardTrue :: PinboardBool
pattern PinboardTrue = MkPinboardBool "yes"

pattern PinboardFalse :: PinboardBool
pattern PinboardFalse = MkPinboardBool "no"

{-# COMPLETE PinboardTrue, PinboardFalse #-}

instance FromJSON PinboardBool where
  parseJSON = Aeson.withText "PinboardBool" (pure . MkPinboardBool)

toBool :: PinboardBool -> Bool
toBool (MkPinboardBool t) = t == "yes"

data PinboardPost = MkPinboardPost
  { href :: Text
  , description :: Text
  , extended :: Text
  , time :: Text
  , tags :: Text
  , shared :: PinboardBool
  , toread :: Maybe PinboardBool
  }
  deriving (Show, Eq, Generic)

epochTimeText :: Text
epochTimeText = Text.pack (Format.formatTime Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (POSIX.posixSecondsToUTCTime 0))

emptyPinboardPost :: PinboardPost
emptyPinboardPost =
  MkPinboardPost
    { href = Null
    , description = Null
    , extended = Null
    , time = epochTimeText
    , tags = Null
    , shared = PinboardFalse
    , toread = Nothing
    }

parseTagString :: Text -> [Text]
parseTagString Null = []
parseTagString str = filter (not . isNull) (map Text.strip (Text.splitOn " " str))

instance FromJSON PinboardPost

parseTime :: (Entity.Error -> e) -> Text -> Either e Time
parseTime fromEntityErr s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack s) of
    Nothing -> Left (fromEntityErr (Entity.InvalidTime s))
    Just utcTime -> Right (MkTime (POSIX.utcTimeToPOSIXSeconds utcTime))

parseTags :: [Text] -> [Label]
parseTags tagList = map (MkLabel . Text.strip) (filter (not . isNull) tagList)

postToEntity :: (Entity.Error -> e) -> PinboardPost -> Either e Entity
postToEntity fromEntityErr post = do
  uri <- Bifunctor.first fromEntityErr (Entity.mkURI post.href)
  createdAt <- parseTime fromEntityErr post.time
  let updatedAt = []
      name = case post.description of
        Null -> Nothing
        desc -> Just (MkName desc)
      names = maybe Set.empty Set.singleton name
      labels = Set.fromList (parseTags (parseTagString post.tags))
      extended = case post.extended of
        Null -> Nothing
        ext -> Just (MkExtended ext)
      shared = toBool post.shared
      toRead = maybe False toBool post.toread
      isFeed = False
      lastVisitedAt = Nothing
  pure Entity.MkEntity {uri, createdAt, updatedAt, names, labels, shared, toRead, isFeed, extended, lastVisitedAt}
