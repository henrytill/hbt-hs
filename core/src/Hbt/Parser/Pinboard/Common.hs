module Hbt.Parser.Pinboard.Common
  ( module Hbt.Pinboard
  , postToEntity
  )
where

import Control.Exception (throwIO)
import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Entity (Entity, Extended (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Pinboard (Post (..))
import Hbt.Pinboard qualified as Pinboard

nonEmpty :: Text -> Maybe Text
nonEmpty t =
  let stripped = Text.strip t
   in if Text.null stripped
        then Nothing
        else Just stripped

toLabel :: Text -> Maybe Label
toLabel t = nonEmpty t <&> MkLabel

postToEntity :: (HasCallStack) => Post -> IO Entity
postToEntity post = do
  uri <- either throwIO pure (URI.parse post.href)
  createdAt <- either throwIO pure (Time.parseRFC3339 post.time)
  let updatedAt = []
      name = post.description >>= nonEmpty <&> MkName
      names = maybe Set.empty Set.singleton name
      labels = Set.fromList (Maybe.mapMaybe toLabel post.tags.unTags)
      extended = post.extended >>= nonEmpty <&> MkExtended
      shared = Pinboard.toBool post.shared
      toRead = Pinboard.toBool post.toread
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
