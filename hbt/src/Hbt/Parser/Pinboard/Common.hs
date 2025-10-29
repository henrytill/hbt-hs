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
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hbt.Entity (Entity, Extended (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Common (IsEmpty (..), pattern Empty)

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
    { href = Empty
    , description = Empty
    , extended = Empty
    , time = epochTimeText
    , tags = Empty
    , shared = PinboardFalse
    , toread = Nothing
    }

parseTags :: Text -> [Label]
parseTags Empty = []
parseTags str =
  let toLabel t =
        let stripped = Text.strip t
         in if isEmpty stripped then Nothing else Just (MkLabel stripped)
   in Maybe.mapMaybe toLabel (Text.words str)

postToEntity :: (HasCallStack) => PinboardPost -> IO Entity
postToEntity post = do
  uri <- either throwIO pure (URI.parse post.href)
  createdAt <- either throwIO pure (Time.parseRFC3339 post.time)
  let updatedAt = []
      name = case post.description of
        Empty -> Nothing
        desc -> Just (MkName desc)
      names = maybe Set.empty Set.singleton name
      labels = Set.fromList (parseTags post.tags)
      extended = case post.extended of
        Empty -> Nothing
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
