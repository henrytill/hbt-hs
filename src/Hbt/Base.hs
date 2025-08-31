{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}

module Hbt.Base
  ( -- * Flow and Format Types
    Flow (..)
  , Format (..)
  , InputFormat
  , OutputFormat

    -- * Format Flow Type Class
  , FormatFlow
  , detectFromExtension
  , supportedFormats

    -- * Lens-style Classes for Options
  , HasFormat (..)
  , setFormat
  )
where

import Data.List (find)
import Data.Proxy (Proxy (..))
import Hbt.Base.TH (deriveAllConstructors)
import Lens.Family2 (Lens', set)

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

$(deriveAllConstructors ''Format ''Flow)

toString :: Format f -> String
toString HTML = "html"
toString JSON = "json"
toString XML = "xml"
toString Markdown = "markdown"
toString YAML = "yaml"

class FormatFlow (f :: Flow) where
  -- | Get all format constructors for this flow direction
  allConstructors :: Proxy f -> [Format f]

  -- | Generate flow-specific error message for invalid format
  formatErrorFlow :: Proxy f -> String -> String

  -- | Detect format from file extension
  detectFromExtension :: String -> Maybe (Format f)

  -- | Parse format string to format type (derived)
  parseFormatFlow :: String -> Maybe (Format f)
  parseFormatFlow s = find (\fmt -> toString fmt == s) (allConstructors (Proxy @f))

instance FormatFlow From where
  allConstructors :: Proxy From -> [Format From]
  allConstructors _ = allFromConstructors

  formatErrorFlow :: Proxy From -> String -> String
  formatErrorFlow _ f = "Invalid input format: " ++ f

  detectFromExtension :: String -> Maybe (Format From)
  detectFromExtension ".html" = Just HTML
  detectFromExtension ".json" = Just JSON
  detectFromExtension ".xml" = Just XML
  detectFromExtension ".md" = Just Markdown
  detectFromExtension _ = Nothing

instance FormatFlow To where
  allConstructors :: Proxy To -> [Format To]
  allConstructors _ = allToConstructors

  formatErrorFlow :: Proxy To -> String -> String
  formatErrorFlow _ f = "Invalid output format: " ++ f

  detectFromExtension :: String -> Maybe (Format To)
  detectFromExtension _ = Nothing -- Output formats can't be detected from files (yet)

-- | Get list of supported format strings
supportedFormats :: forall f -> (FormatFlow f) => [String]
supportedFormats f = map toString (allConstructors (Proxy @f))

-- | Lens-style class for types that have a format field for a given flow
class HasFormat (f :: Flow) s where
  format :: Lens' s (Maybe (Format f))

-- | TODO
setFormat :: forall f -> (FormatFlow f, HasFormat f s) => String -> s -> s
setFormat f str opts
  | Just fmt <- parseFormatFlow @f str = set format (Just fmt) opts
  | otherwise = error $ formatErrorFlow (Proxy @f) str
