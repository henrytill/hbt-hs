{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Formatter.HTML
  ( format
  )
where

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
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
import Hbt.Entity (Entity (..), Extended (..), Label (..), Name (..), getIsFeed, getLastVisitedAt, getShared, getToRead)
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Text.Microstache (Template)
import Text.Microstache qualified as Microstache

data TemplateEntity = MkTemplateEntity
  { href :: Text
  , addDate :: Text
  , title :: Text
  , lastModified :: Maybe Text
  , tags :: Maybe Text
  , private :: Maybe Text
  , toRead :: Maybe Text
  , feed :: Maybe Text
  , lastVisit :: Maybe Text
  , description :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

getFirstName :: Text -> Set Name -> Text
getFirstName def names
  | Set.null names = def
  | otherwise = (Set.findMin names).unName

-- | The most recent update, if the entity has one.
--
-- @createdAt@ is the minimum of @updatedAt@ rather than a field of its own, so
-- it has to come out of the set before the maximum means anything: without
-- that, an entity that was never updated reports its creation time as its last
-- modification. This mirrors what 'Hbt.Entity.toJSON' writes.
getLastModified :: Entity -> Maybe Time
getLastModified entity = Set.lookupMax (Set.delete entity.createdAt entity.updatedAt)

-- | Escape a value destined for a double-quoted attribute.
--
-- The double quote delimits the value, so it has to go; the single quote is
-- safe there, and @&apos;@ is not HTML 4.
escapeAttribute :: Text -> Text
escapeAttribute =
  Text.replace "\"" "&quot;"
    . Text.replace ">" "&gt;"
    . Text.replace "<" "&lt;"
    . Text.replace "&" "&amp;"

-- | Escape a value destined for text content, where quotes are safe.
escapeText :: Text -> Text
escapeText =
  Text.replace ">" "&gt;"
    . Text.replace "<" "&lt;"
    . Text.replace "&" "&amp;"

stringOfBool :: Bool -> Text
stringOfBool False = "0"
stringOfBool True = "1"

feedOfBool :: Bool -> Text
feedOfBool False = "false"
feedOfBool True = "true"

fromEntity :: Entity -> TemplateEntity
fromEntity entity =
  MkTemplateEntity
    { href = escapeAttribute href
    , addDate = Time.toText entity.createdAt
    , title = escapeText (getFirstName href entity.names)
    , lastModified = fmap Time.toText (getLastModified entity)
    , tags = if null tagsList then Nothing else Just (escapeAttribute (Text.intercalate "," tagsList))
    , private = fmap (stringOfBool . not) (getShared entity.shared)
    , toRead = fmap stringOfBool (getToRead entity.toRead)
    , feed = fmap feedOfBool (getIsFeed entity.isFeed)
    , lastVisit = fmap Time.toText (getLastVisitedAt entity.lastVisitedAt)
    , description = fmap (escapeText . (.unExtended)) (Maybe.listToMaybe entity.extended)
    }
  where
    href = Maybe.fromMaybe mempty (URI.toText entity.uri) -- TODO
    tagsList = List.sort (coerce (Set.toList entity.labels))

format :: Template -> Collection -> Text
format template collection = LazyText.toStrict (Microstache.renderMustache template toRender)
  where
    f e acc = fromEntity e : acc
    templateEntities = Vector.foldr f [] (Collection.allEntities collection)
    toRender = Aeson.object ["entities" .= templateEntities]
