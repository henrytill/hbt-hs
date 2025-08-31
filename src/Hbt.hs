{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeData #-}

module Hbt
  ( Flow (..)
  , Format (..)
  , InputFormat
  , OutputFormat
  , parseWith
  , formatWith
  , SomeParseError (..)
  , SomeFormatError
  )
where

import Control.Exception (SomeException, handle)
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

parseWith :: Format From -> Text -> Either SomeParseError Collection
parseWith HTML = first SomeParseError . HTMLParser.parse
parseWith JSON = first SomeParseError . PinboardJSON.parse
parseWith XML = first SomeParseError . PinboardXML.parse
parseWith Markdown = first SomeParseError . Markdown.parse "content"

type SomeFormatError = SomeException

withFormatError :: IO Text -> IO (Either SomeFormatError Text)
withFormatError action = handle handler (fmap Right action)
  where
    handler :: SomeException -> IO (Either SomeException a)
    handler = return . Left

formatWith :: Format To -> Collection -> IO (Either SomeFormatError Text)
formatWith YAML collection = pure . Right . Text.decodeUtf8 $ YamlPretty.encodePretty yamlConfig collection
formatWith HTML collection = withFormatError $ do
  template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
  return $ HTMLFormatter.format template collection
