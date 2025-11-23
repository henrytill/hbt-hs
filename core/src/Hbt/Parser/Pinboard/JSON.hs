{-# LANGUAGE DeriveAnyClass #-}

module Hbt.Parser.Pinboard.JSON (Error (..), parse) where

import Control.Exception (Exception, throwIO)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection

newtype Error = ParseError String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

parse :: (HasCallStack) => Text -> IO Collection
parse input = do
  posts <- either (throwIO . ParseError) pure (Aeson.eitherDecodeStrict (Text.encodeUtf8 input))
  Collection.fromPosts posts
