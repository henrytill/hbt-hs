{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Hbt.Class
  ( Parser (..)
  , Formatter (..)
  , parseWith
  , formatWith
  , parseDispatch
  , formatDispatch
  , SomeParseError (..)
  , SomeFormatError (..)
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Yaml.Pretty qualified as YamlPretty
import Hbt.Base (Flow (..), Format (..))
import Hbt.Collection (Collection, yamlConfig)
import Hbt.Formatter.HTML qualified as HTMLFormatter
import Hbt.Parser.HTML qualified as HTMLParser
import Hbt.Parser.Markdown qualified as Markdown
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import Text.Microstache (Template, compileMustacheFile)

class (Show e) => Error e

class (Error (ParseError f)) => Parser (f :: Format From) where
  type ParseError f :: Type
  parseContent :: Proxy f -> Text -> Either (ParseError f) Collection

class (Error (FormatError f)) => Formatter (f :: Format To) where
  type FormatError f :: Type
  type FormatContext f :: Type
  formatContent :: Proxy f -> FormatContext f -> Collection -> Either (FormatError f) Text

instance Parser HTML where
  type ParseError HTML = HTMLParser.Error
  parseContent _ = HTMLParser.parse

instance Parser JSON where
  type ParseError JSON = PinboardJSON.Error
  parseContent _ = PinboardJSON.parse

instance Parser XML where
  type ParseError XML = PinboardXML.Error
  parseContent _ = PinboardXML.parse

instance Parser Markdown where
  type ParseError Markdown = Markdown.Error
  parseContent _ content = Markdown.parse "content" content

instance Formatter YAML where
  type FormatError YAML = String
  type FormatContext YAML = ()
  formatContent _ _ = Right . Text.decodeUtf8 . YamlPretty.encodePretty yamlConfig

instance Formatter HTML where
  type FormatError HTML = String
  type FormatContext HTML = Template
  formatContent _ template collection = Right $ HTMLFormatter.format template collection

instance Error String

instance Error HTMLParser.Error

instance Error PinboardJSON.Error

instance Error PinboardXML.Error

instance Error Markdown.Error

data SomeParseError = forall e. (Error e) => SomeParseError e

instance Show SomeParseError where
  show (SomeParseError e) = show e

data SomeFormatError = forall e. (Error e) => SomeFormatError e

instance Show SomeFormatError where
  show (SomeFormatError e) = show e

parseWith :: forall f -> (Parser f) => Text -> Either (ParseError f) Collection
parseWith f content = parseContent (Proxy @f) content

formatWith :: forall f -> (Formatter f) => FormatContext f -> Collection -> Either (FormatError f) Text
formatWith f ctx collection = formatContent (Proxy @f) ctx collection

parseDispatch :: Format From -> Text -> Either SomeParseError Collection
parseDispatch HTML = first SomeParseError . parseWith HTML
parseDispatch JSON = first SomeParseError . parseWith JSON
parseDispatch XML = first SomeParseError . parseWith XML
parseDispatch Markdown = first SomeParseError . parseWith Markdown

formatDispatch :: Format To -> Collection -> IO (Either SomeFormatError Text)
formatDispatch YAML collection = return . first SomeFormatError $ formatWith YAML () collection
formatDispatch HTML collection = do
  template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
  return . first SomeFormatError $ formatWith HTML template collection
