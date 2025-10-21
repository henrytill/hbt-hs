{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Parser.Pinboard.Common
  ( PinboardBool
  , pattern PinboardTrue
  , pattern PinboardFalse
  , PinboardPost (..)
  , emptyPinboardPost
  , postToEntity
  )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hbt.Entity (Entity, Error, Extended (..), Label (..), Name (..), Time (..))
import Hbt.Entity qualified as Entity
import Hbt.Parser.Common (IsNull (..), pattern Null)

newtype PinboardBool = MkPinboardBool Text
  deriving (Show, Eq)

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

instance FromJSON PinboardPost

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

parseTime :: (HasCallStack) => Text -> Either Error Time
parseTime s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack s) of
    Nothing -> Left (Entity.InvalidTime s)
    Just utcTime -> pure (MkTime (POSIX.utcTimeToPOSIXSeconds utcTime))

parseTags :: [Text] -> [Label]
parseTags tagList = map (MkLabel . Text.strip) (filter (not . isNull) tagList)

postToEntity :: (HasCallStack) => PinboardPost -> IO Entity
postToEntity post = do
  uri <- either throwIO pure (Entity.mkURI post.href)
  createdAt <- either throwIO pure (parseTime post.time)
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
  pure
    Entity.MkEntity
      { uri
      , createdAt
      , updatedAt
      , names
      , labels
      , shared
      , toRead
      , isFeed
      , extended
      , lastVisitedAt
      }
