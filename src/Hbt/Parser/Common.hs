{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.Common
  ( -- * IsNull type class and Null pattern
    IsNull (..)
  , pattern Null

    -- * Attribute patterns
  , pattern Href
  , pattern Description
  , pattern Extended
  , pattern Time
  , pattern AddDate
  , pattern LastModified
  , pattern LastVisit
  , pattern Tags
  , pattern Private
  , pattern Shared
  , pattern ToRead
  , pattern Feed

    -- * Value patterns
  , pattern One
  , pattern STrue
  , pattern Yes

    -- * Helper functions
  , matchAttr
  , matchAttrTagList
  , parseFileWithParser

    -- * Parser monad
  , ParserMonad
  , runParserMonad
  )
where

import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hbt.Collection.Entity (URI (..))
import Network.URI qualified as URI
import Text.HTML.TagSoup (Attribute)

class IsNull s where
  isNull :: s -> Bool

instance IsNull [a] where
  isNull = null

instance IsNull Text where
  isNull = Text.null

instance IsNull URI.URI where
  isNull uri = uri == URI.nullURI

instance IsNull URI where
  isNull (MkURI uri) = isNull uri

pattern Null :: (IsNull s, Monoid s) => s
pattern Null <- (isNull -> True)
  where
    Null = mempty

pattern Href :: Text -> Attribute Text
pattern Href value <- (matchAttr "href" -> Just value)

pattern Description :: Text -> Attribute Text
pattern Description value <- (matchAttr "description" -> Just value)

pattern Extended :: Text -> Attribute Text
pattern Extended value <- (matchAttr "extended" -> Just value)

pattern Time :: Text -> Attribute Text
pattern Time value <- (matchAttr "time" -> Just value)

pattern AddDate :: Text -> Attribute Text
pattern AddDate value <- (matchAttr "add_date" -> Just value)

pattern LastModified :: Text -> Attribute Text
pattern LastModified value <- (matchAttr "last_modified" -> Just value)

pattern LastVisit :: Text -> Attribute Text
pattern LastVisit value <- (matchAttr "last_visit" -> Just value)

pattern Private :: Text -> Attribute Text
pattern Private value <- (matchAttr "private" -> Just value)

pattern Shared :: Text -> Attribute Text
pattern Shared value <- (matchAttr "shared" -> Just value)

pattern ToRead :: Text -> Attribute Text
pattern ToRead value <- (matchAttr "toread" -> Just value)

pattern Feed :: Text -> Attribute Text
pattern Feed value <- (matchAttr "feed" -> Just value)

pattern Tags :: [Text] -> Attribute Text
pattern Tags values <- (matchAttrTagList -> Just values)

pattern One :: Text
pattern One = "1"

pattern STrue :: Text
pattern STrue = "true"

pattern Yes :: Text
pattern Yes = "yes"

matchAttr :: Text -> Attribute Text -> Maybe Text
matchAttr key (attrKey, attrValue)
  | Text.toLower attrKey == Text.toLower key = if isNull attrValue then Nothing else Just attrValue
  | otherwise = Nothing

parseTagStringWith :: Text -> Text -> [Text]
parseTagStringWith separator value
  | isNull value = []
  | otherwise = filter (not . isNull) (map Text.strip (Text.splitOn separator value))

-- (Pinboard) XML format uses "tag" with space-separated values: tag="web programming haskell"
-- HTML format uses "tags" with comma-separated values: TAGS="web,programming,haskell"
matchAttrTagList :: Attribute Text -> Maybe [Text]
matchAttrTagList (attrKey, attrValue)
  | Text.toLower attrKey == "tag" = Just (parseTagStringWith " " attrValue)
  | Text.toLower attrKey == "tags" = Just (parseTagStringWith "," attrValue)
  | otherwise = Nothing

parseFileWithParser :: (Text -> Either e a) -> FilePath -> IO (Either e a)
parseFileWithParser parser filepath = do
  content <- Text.readFile filepath
  pure (parser content)

type ParserMonad s e = StateT s (Either e)

runParserMonad :: ParserMonad s e a -> s -> Either e (a, s)
runParserMonad = State.runStateT
