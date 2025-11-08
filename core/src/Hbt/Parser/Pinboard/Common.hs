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
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Entity (Entity, Extended (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Pinboard

toLabel :: Text -> Maybe Label
toLabel t =
  let stripped = Text.strip t
   in if Text.null stripped
        then Nothing
        else Just (MkLabel stripped)

parseTags :: Text -> [Label]
parseTags str
  | Text.null str = []
  | otherwise = Maybe.mapMaybe toLabel (Text.words str)

postToEntity :: (HasCallStack) => PinboardPost -> IO Entity
postToEntity post = do
  uri <- either throwIO pure (URI.parse post.href)
  createdAt <- either throwIO pure (Time.parseRFC3339 post.time)
  let updatedAt = []
      name = case post.description of
        desc | Text.null desc -> Nothing
        desc -> Just (MkName desc)
      names = maybe Set.empty Set.singleton name
      labels = Set.fromList (parseTags post.tags)
      extended = case post.extended of
        ext | Text.null ext -> Nothing
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
