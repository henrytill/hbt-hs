{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Common where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, runStateT)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.HTML.TagSoup (Attribute)

lookupAttr :: Text -> [Attribute Text] -> Maybe Text
lookupAttr key attrs = lookup (Text.toLower key) (map (first Text.toLower) attrs)

attrOrDefault :: Text -> Text -> [Attribute Text] -> Text
attrOrDefault key def attrs = maybe def id (lookupAttr key attrs)

attrOrEmpty :: Text -> [Attribute Text] -> Text
attrOrEmpty key attrs = attrOrDefault key mempty attrs

attrMatches :: Text -> Text -> [Attribute Text] -> Bool
attrMatches key expected attrs = lookupAttr key attrs == Just expected

parseFileWithParser :: (Text -> Either e a) -> FilePath -> IO (Either e a)
parseFileWithParser parser filepath = do
  content <- readFile filepath
  return $ parser (Text.pack content)

type ParserMonad s e = StateT s (Except e)

runParserMonad :: ParserMonad s e a -> s -> Either e (a, s)
runParserMonad m s = runExcept (runStateT m s)
