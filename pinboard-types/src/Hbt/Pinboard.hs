{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Pinboard
  ( PinboardBool
  , pattern PinboardTrue
  , pattern PinboardFalse
  , toBool
  , PinboardPost (..)
  , emptyPinboardPost
  )
where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import GHC.Generics (Generic)

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
    { href = Text.empty
    , description = Text.empty
    , extended = Text.empty
    , time = epochTimeText
    , tags = Text.empty
    , shared = PinboardFalse
    , toread = Nothing
    }
