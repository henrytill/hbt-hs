{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.Common
  ( -- * IsNull type class and Null pattern
    IsNull (..)
  , pattern Null

    -- * Attribute
  , Attribute

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

    -- * StateIO
  , StateIO
  , runStateIO

    -- * Misc
  , drop1
  )
where

import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hbt.Entity (URI (..), nullURI)

class IsNull s where
  isNull :: s -> Bool

instance IsNull Text where
  isNull = Text.null

instance IsNull ByteString where
  isNull = ByteString.null

instance IsNull URI where
  isNull uri = uri == nullURI

pattern Null :: (IsNull s, Monoid s) => s
pattern Null <- (isNull -> True)
  where
    Null = mempty

type Attribute = (ByteString, ByteString)

pattern Href :: Text -> Attribute
pattern Href value <- (matchAttr "href" -> Just value)

pattern Description :: Text -> Attribute
pattern Description value <- (matchAttr "description" -> Just value)

pattern Extended :: Text -> Attribute
pattern Extended value <- (matchAttr "extended" -> Just value)

pattern Time :: Text -> Attribute
pattern Time value <- (matchAttr "time" -> Just value)

pattern AddDate :: Text -> Attribute
pattern AddDate value <- (matchAttr "add_date" -> Just value)

pattern LastModified :: Text -> Attribute
pattern LastModified value <- (matchAttr "last_modified" -> Just value)

pattern LastVisit :: Text -> Attribute
pattern LastVisit value <- (matchAttr "last_visit" -> Just value)

pattern Private :: Text -> Attribute
pattern Private value <- (matchAttr "private" -> Just value)

pattern Shared :: Text -> Attribute
pattern Shared value <- (matchAttr "shared" -> Just value)

pattern ToRead :: Text -> Attribute
pattern ToRead value <- (matchAttr "toread" -> Just value)

pattern Feed :: Text -> Attribute
pattern Feed value <- (matchAttr "feed" -> Just value)

pattern Tags :: [Text] -> Attribute
pattern Tags values <- (matchAttrTagList -> Just values)

pattern One :: Text
pattern One = "1"

pattern STrue :: Text
pattern STrue = "true"

pattern Yes :: Text
pattern Yes = "yes"

toLower :: ByteString -> ByteString
-- toLower bs = Char8.map Char.toLower bs
toLower bs = bs

matchAttr :: ByteString -> Attribute -> Maybe Text
matchAttr key (attrKey, attrValue)
  | toLower attrKey == toLower key
  , not (isNull attrValue) =
      Just (Text.decodeUtf8 attrValue)
  | otherwise = Nothing

parseTagStringWith :: ByteString -> ByteString -> [Text]
parseTagStringWith separator value
  | isNull value = []
  | otherwise = filter (not . isNull) (map (Text.decodeUtf8 . Char8.strip) (Char8.split (Char8.head separator) value))

-- (Pinboard) XML format uses "tag" with space-separated values: tag="web programming haskell"
-- HTML format uses "tags" with comma-separated values: TAGS="web,programming,haskell"
matchAttrTagList :: Attribute -> Maybe [Text]
matchAttrTagList (attrKey, attrValue) =
  case toLower attrKey of
    "tag" -> Just (parseTagStringWith " " attrValue)
    "tags" -> Just (parseTagStringWith "," attrValue)
    _ -> Nothing

type StateIO s = StateT s IO

runStateIO :: StateIO s a -> s -> IO (a, s)
runStateIO = State.runStateT

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs
