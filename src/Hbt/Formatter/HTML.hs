{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Formatter.HTML
  ( format
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity (..), Extended (..), Label (..), Name (..), Time (..), URI (..))
import Text.Microstache (Template, renderMustache)

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
  deriving (Generic)

instance ToJSON TemplateEntity

format :: Template -> Collection -> Text
format template collection =
  LazyText.toStrict . renderMustache template $
    object ["entities" .= map toTemplateEntity (Vector.toList $ Collection.allEntities collection)]

toTemplateEntity :: Entity -> TemplateEntity
toTemplateEntity entity =
  let MkURI uriVal = entity.uri
      uriText = Text.pack $ show uriVal
      tagsList = sort . map (.unLabel) $ Set.toList entity.labels
      tagsText = Text.intercalate "," tagsList
   in MkTemplateEntity
        { uri = uriText
        , createdAt = formatTime entity.createdAt
        , title = getFirstName uriText entity.names
        , lastModified = fmap formatTime (getLastModified entity)
        , tags = if null tagsList then Nothing else Just tagsText
        , shared = entity.shared
        , toRead = entity.toRead
        , isFeed = entity.isFeed
        , lastVisit = fmap formatTime entity.lastVisitedAt
        , description = fmap (.unExtended) entity.extended
        }

getFirstName :: Text -> Set Name -> Text
getFirstName def names
  | Set.null names = def
  | otherwise = (.unName) $ Set.findMin names

getLastModified :: Entity -> Maybe Time
getLastModified = listToMaybe . (.updatedAt)

formatTime :: Time -> Text
formatTime (MkTime posixTime) = Text.pack . show @Integer $ round posixTime
