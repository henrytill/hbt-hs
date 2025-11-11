{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Pinboard.Post
  ( Tags (unTags)
  , mkTags
  , Post (..)
  , empty
  )
where

import Data.Aeson (FromJSON (..), withText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import GHC.Generics (Generic, Generically (..))
import Hbt.Pinboard.Bool qualified as Pinboard (Bool, pattern False)

newtype Tags = MkTags {unTags :: [Text]}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

mkTags :: Text -> Tags
mkTags = MkTags . Text.words

instance FromJSON Tags where
  parseJSON = withText "Tags" (pure . mkTags)

data Post = MkPost
  { href :: Text
  , description :: Maybe Text
  , extended :: Maybe Text
  , time :: Text
  , tags :: Tags
  , meta :: Maybe Text
  , hash :: Maybe Text
  , shared :: Pinboard.Bool
  , toread :: Pinboard.Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via Generically Post

epochTimeText :: Text
epochTimeText = Text.pack (Format.formatTime Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (POSIX.posixSecondsToUTCTime 0))

empty :: Post
empty =
  MkPost
    { href = Text.empty
    , description = Nothing
    , extended = Nothing
    , time = epochTimeText
    , tags = mempty
    , meta = Nothing
    , hash = Nothing
    , shared = Pinboard.False
    , toread = Pinboard.False
    }
