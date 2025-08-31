{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeData #-}

module Hbt
  ( -- * Core Types
    Flow (..)
  , Format (..)
  , InputFormat
  , OutputFormat

    -- * Dispatch Functions
  , parseDispatch
  , formatDispatch
  , SomeParseError (..)
  , SomeFormatError (..)
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Yaml.Pretty qualified as YamlPretty
import Hbt.Collection (Collection, yamlConfig)
import Hbt.Formatter.HTML qualified as HTMLFormatter
import Hbt.Parser.HTML qualified as HTMLParser
import Hbt.Parser.Markdown qualified as Markdown
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import Text.Microstache (compileMustacheFile)

type data Flow = From | To

data Format (f :: Flow) where
  JSON :: Format From
  XML :: Format From
  Markdown :: Format From
  HTML :: Format f
  YAML :: Format To

deriving instance Show (Format f)

deriving instance Eq (Format f)

type InputFormat = Format From

type OutputFormat = Format To

data SomeParseError = forall e. (Show e) => SomeParseError e

instance Show SomeParseError where
  show (SomeParseError e) = show e

data SomeFormatError = forall e. (Show e) => SomeFormatError e

instance Show SomeFormatError where
  show (SomeFormatError e) = show e

parseDispatch :: Format From -> Text -> Either SomeParseError Collection
parseDispatch HTML content = first SomeParseError $ HTMLParser.parse content
parseDispatch JSON content = first SomeParseError $ PinboardJSON.parse content
parseDispatch XML content = first SomeParseError $ PinboardXML.parse content
parseDispatch Markdown content = first SomeParseError $ Markdown.parse "content" content

formatDispatch :: Format To -> Collection -> IO (Either SomeFormatError Text)
formatDispatch YAML collection =
  return $ Right $ Text.decodeUtf8 $ YamlPretty.encodePretty yamlConfig collection
formatDispatch HTML collection = do
  template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
  return $ Right $ HTMLFormatter.format template collection
