{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.JSON where

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Parser.Common (parseFileWithParser)
import Hbt.Parser.Pinboard.Common (Error (..), PinboardPost, postToEntity)

data JSONError
  = ParseError String
  | PinboardError Error
  deriving (Show, Eq)

postsToCollection :: [PinboardPost] -> Either JSONError Collection
postsToCollection posts = do
  entities <- traverse (first PinboardError . postToEntity) (reverse posts)
  pure $ Collection.fromEntities entities

parse :: Text -> Either JSONError Collection
parse input = do
  posts <- first ParseError $ Aeson.eitherDecode $ ByteString.fromStrict $ Text.encodeUtf8 input
  postsToCollection posts

parseFile :: FilePath -> IO (Either JSONError Collection)
parseFile = parseFileWithParser parse
