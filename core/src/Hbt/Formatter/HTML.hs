{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Formatter.HTML
  ( format
  )
where

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Extended (..), Label (..), Name (..))
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Text.Microstache (Template)
import Text.Microstache qualified as Microstache

data TemplateEntity = MkTemplateEntity
  { uri :: Text
  , createdAt :: Text
  , title :: Text
  , lastModified :: Maybe Text
  , tags :: Maybe Text
  , shared :: Bool
  , toRead :: Bool
  , isFeed :: Bool
  , lastVisit :: Maybe Text
  , description :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

getFirstName :: Text -> Set Name -> Text
getFirstName def names
  | Set.null names = def
  | otherwise = (Set.findMin names).unName

getLastModified :: Entity -> Maybe Time
getLastModified entity = Maybe.listToMaybe entity.updatedAt

toTemplateEntity :: Entity -> TemplateEntity
toTemplateEntity entity =
  let uriText = URI.toText entity.uri
      tagsList = List.sort (map (.unLabel) (Set.toList entity.labels))
      tagsText = Text.intercalate "," tagsList
   in MkTemplateEntity
        { uri = uriText
        , createdAt = Time.toText entity.createdAt
        , title = getFirstName uriText entity.names
        , lastModified = fmap Time.toText (getLastModified entity)
        , tags = if null tagsList then Nothing else Just tagsText
        , shared = entity.shared
        , toRead = entity.toRead
        , isFeed = entity.isFeed
        , lastVisit = fmap Time.toText entity.lastVisitedAt
        , description = fmap (.unExtended) entity.extended
        }

format :: Template -> Collection -> Text
format template collection =
  let f e acc = toTemplateEntity e : acc
      templateEntities = Vector.foldr f [] (Collection.allEntities collection)
      toRender = Aeson.object ["entities" .= templateEntities]
   in LazyText.toStrict (Microstache.renderMustache template toRender)
