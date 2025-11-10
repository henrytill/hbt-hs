{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Pinboard.Post where

import Data.Aeson (FromJSON (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import GHC.Generics (Generic)
import Hbt.Pinboard.Bool qualified as Pinboard (Bool, pattern False)

data Post = MkPost
  { href :: Text
  , description :: Text
  , extended :: Text
  , time :: Text
  , tags :: Text
  , shared :: Pinboard.Bool
  , toread :: Maybe Pinboard.Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Post

epochTimeText :: Text
epochTimeText = Text.pack (Format.formatTime Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (POSIX.posixSecondsToUTCTime 0))

empty :: Post
empty =
  MkPost
    { href = Text.empty
    , description = Text.empty
    , extended = Text.empty
    , time = epochTimeText
    , tags = Text.empty
    , shared = Pinboard.False
    , toread = Nothing
    }
