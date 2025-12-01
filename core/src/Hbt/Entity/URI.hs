{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Entity.URI
  ( Error
  , URI
  , empty
  , null
  , parse
  , toText
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad.Except (MonadError, liftEither, runExcept)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Maybe qualified as Maybe
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Generics (Generic)
import URI.ByteString (URIParseError)
import URI.ByteString qualified as URI
import Prelude hiding (null)

data Error
  = InvalidURI URIParseError Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype URI = MkURI {unURI :: First (URI.URIRef URI.Absolute)}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid)

mkURI :: URI.URIRef URI.Absolute -> URI
mkURI = MkURI . First . Just

empty :: URI
empty = mempty

null :: URI -> Bool
null = (==) empty

translate :: Text -> Text
translate uriText =
  let (beforeQuery, afterQuery) = Text.breakOn "?" uriText
   in if Text.null afterQuery
        then uriText
        else beforeQuery <> Text.replace ";" "&" afterQuery

normalizeURI :: URI.URIRef URI.Absolute -> URI.URIRef URI.Absolute
normalizeURI uri
  | URI.schemeBS (URI.uriScheme uri) `elem` ["http", "https"]
  , URI.uriPath uri == mempty
  , Maybe.isJust (URI.uriAuthority uri) =
      uri {URI.uriPath = "/"}
  | otherwise = uri

parseURI :: Text -> Either URIParseError (URI.URIRef URI.Absolute)
parseURI = URI.parseURI URI.laxURIParserOptions . Text.Encoding.encodeUtf8

-- Lift single error into list to enable Alternative composition
liftSingle :: (MonadError [e] m) => (Either e a) -> m a
liftSingle = liftEither . Bifunctor.first (: [])

parse :: Text -> Either Error URI
parse text =
  let tryOriginal = liftSingle (parseURI text)
      tryTranslated = liftSingle (parseURI (translate text))
   in case runExcept (tryOriginal <|> tryTranslated) of
        Left [] -> error "Impossible: both URI parsing attempts failed but no errors recorded"
        Left (err : _) -> Left (InvalidURI err text)
        Right uri -> Right (mkURI (normalizeURI uri))

toText :: URI -> Maybe Text
toText (MkURI (First maybeURI)) = fmap (Text.Encoding.decodeUtf8 . URI.serializeURIRef') maybeURI

instance ToJSON URI where
  toJSON = toJSON . toText

instance FromJSON URI where
  parseJSON = Aeson.withText "URI" $ \t ->
    either (fail . show) pure (parse t)
