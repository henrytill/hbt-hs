{-# LANGUAGE OverloadedStrings #-}

module Hbt.Formatter.HTML
  ( format
  )
where

import Data.Aeson (Value (..), object, (.=))
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Vector qualified as Vector
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity (..), Extended (..), Label (..), Name (..), Time (..), URI (..))
import Text.Microstache (Template, renderMustache)

-- | Format a Collection as an HTML string in Netscape Bookmark format
format :: Template -> Collection -> Text
format template collection = LText.toStrict $ renderMustache template templateData
  where
    templateData = object ["entities" .= map entityToJSON (Vector.toList $ Collection.allEntities collection)]

-- | Convert an Entity to JSON for template rendering
entityToJSON :: Entity -> Value
entityToJSON entity@(MkEntity uri createdAt _ names labels shared toRead isFeed extended lastVisitedAt) =
  object $
    catMaybes
      [ Just ("uri" .= uriText)
      , Just ("createdAt" .= formatTime createdAt)
      , Just ("title" .= getFirstName names)
      , fmap (\t -> "lastModified" .= formatTime t) (getLastModified entity)
      , if null tagsList then Nothing else Just ("tags" .= tagsText)
      , Just ("shared" .= shared)
      , Just ("toRead" .= toRead)
      , Just ("isFeed" .= isFeed)
      , fmap (\t -> "lastVisit" .= formatTime t) lastVisitedAt
      , fmap (\(MkExtended desc) -> "description" .= desc) extended
      ]
  where
    MkURI uriVal = uri
    uriText = Text.pack $ show uriVal
    tagsList = sort $ map (\(MkLabel lbl) -> lbl) $ Set.toList labels
    tagsText = Text.intercalate "," tagsList

-- | Get the first name alphabetically, or use the URI if no names
getFirstName :: Set.Set Name -> Text
getFirstName names
  | Set.null names = ""
  | otherwise = let (MkName name) = Set.findMin names in name

-- | Get the last modified time (first in updatedAt list)
getLastModified :: Entity -> Maybe Time
getLastModified (MkEntity _ _ updatedAt _ _ _ _ _ _ _) = case updatedAt of
  [] -> Nothing
  (t : _) -> Just t

-- | Format a time as Unix timestamp
formatTime :: Time -> Text
formatTime (MkTime posixTime) = Text.pack $ show (round posixTime :: Integer)
