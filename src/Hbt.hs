{-# LANGUAGE GADTs #-}
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
  , SomeParseError (..)
  , SomeFormatError
  )
where

import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Yaml.Pretty qualified as YamlPretty
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

type InputFormat = Format From

type OutputFormat = Format To

allInputFormats :: [InputFormat]
allInputFormats = [JSON, XML, Markdown, HTML]

allOutputFormats :: [OutputFormat]
allOutputFormats = [HTML, YAML]

data SomeParseError = forall e. (Show e) => SomeParseError e

instance Show SomeParseError where
  show (SomeParseError e) = show e

parseWith :: Format From -> Text -> Either SomeParseError Collection
parseWith JSON input = Bifunctor.first SomeParseError (PinboardJSON.parse input)
parseWith XML input = Bifunctor.first SomeParseError (PinboardXML.parse input)
parseWith Markdown input = Bifunctor.first SomeParseError (Markdown.parse "content" input)
parseWith HTML input = Bifunctor.first SomeParseError (HTMLParser.parse input)

type SomeFormatError = SomeException

withFormatError :: IO Text -> IO (Either SomeFormatError Text)
withFormatError action =
  let handler :: SomeException -> IO (Either SomeException a)
      handler e = pure (Left e)
   in Exception.handle handler (fmap Right action)

formatWith :: Format To -> Collection -> IO (Either SomeFormatError Text)
formatWith YAML collection = pure (Right (Text.decodeUtf8 (YamlPretty.encodePretty Collection.yamlConfig collection)))
formatWith HTML collection = withFormatError $ do
  template <- Microstache.compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
  pure (HTMLFormatter.format template collection)
