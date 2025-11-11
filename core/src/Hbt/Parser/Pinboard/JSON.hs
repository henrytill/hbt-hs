{-# LANGUAGE DeriveAnyClass #-}

module Hbt.Parser.Pinboard.JSON (Error (..), parse) where

import Control.Exception (Exception, throwIO)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Parser.Pinboard.Common (postToEntity)
import Hbt.Pinboard (Post)

newtype Error = ParseError String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

postsToCollection :: [Post] -> IO Collection
postsToCollection posts = do
  entities <- traverse postToEntity (reverse posts)
  pure (Collection.fromEntities entities)

parse :: (HasCallStack) => Text -> IO Collection
parse input = do
  posts <- either (throwIO . ParseError) pure (Aeson.eitherDecodeStrict (Text.encodeUtf8 input))
  postsToCollection posts
