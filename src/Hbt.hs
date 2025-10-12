{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}

module Hbt
  ( Flow (..)
  , Format (..)
  , InputFormat
  , OutputFormat
  , allInputFormats
  , allOutputFormats
  , parseWith
  , formatWith
  , toString
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed qualified as FileEmbed
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Yaml.Pretty qualified as YamlPretty
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Formatter.HTML qualified as HTMLFormatter
import Hbt.Parser.HTML qualified as HTMLParser
import Hbt.Parser.Markdown qualified as Markdown
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import Text.Microstache qualified as Microstache

type data Flow = From | To

data Format (f :: Flow) where
  JSON :: Format From
  XML :: Format From
  Markdown :: Format From
  HTML :: Format f
  YAML :: Format To

deriving instance Show (Format f)

deriving instance Eq (Format f)

deriving instance Ord (Format f)

type InputFormat = Format From

type OutputFormat = Format To

allInputFormats :: [InputFormat]
allInputFormats = [JSON, XML, Markdown, HTML]

allOutputFormats :: [OutputFormat]
allOutputFormats = [HTML, YAML]

toString :: Format f -> String
toString JSON = "json"
toString XML = "xml"
toString Markdown = "markdown"
toString HTML = "html"
toString YAML = "yaml"

parseWith :: (HasCallStack) => Format From -> Text -> IO Collection
parseWith JSON = PinboardJSON.parse
parseWith XML = PinboardXML.parse
parseWith Markdown = Markdown.parse "content"
parseWith HTML = HTMLParser.parse

templateBytes :: ByteString
templateBytes = $(FileEmbed.embedFileRelative "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache")

formatWith :: (HasCallStack) => Format To -> Collection -> IO Text
formatWith YAML collection =
  pure (Text.decodeUtf8 (YamlPretty.encodePretty Collection.yamlConfig collection))
formatWith HTML collection = do
  let templateText = LazyText.fromStrict (Text.decodeUtf8 templateBytes)
  template <- either (fail . show) pure $ Microstache.compileMustacheText "netscape_bookmarks" templateText
  pure (HTMLFormatter.format template collection)
