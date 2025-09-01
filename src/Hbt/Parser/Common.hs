module Hbt.Parser.Common where

import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor qualified as Bifunctor
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Text.HTML.TagSoup (Attribute)

lookupAttr :: Text -> [Attribute Text] -> Maybe Text
lookupAttr key attrs = lookup (Text.toLower key) (map (Bifunctor.first Text.toLower) attrs)

attrOrDefault :: Text -> Text -> [Attribute Text] -> Text
attrOrDefault key def attrs = Maybe.fromMaybe def (lookupAttr key attrs)

attrOrEmpty :: Text -> [Attribute Text] -> Text
attrOrEmpty key = attrOrDefault key mempty

attrMatches :: Text -> Text -> [Attribute Text] -> Bool
attrMatches key expected attrs = lookupAttr key attrs == Just expected

requireAttr :: Text -> [Attribute Text] -> Maybe Text
requireAttr key attrs =
  case lookupAttr key attrs of
    Nothing -> Nothing
    Just value
      | Text.null value -> Nothing
      | otherwise -> Just value

parseFileWithParser :: (Text -> Either e a) -> FilePath -> IO (Either e a)
parseFileWithParser parser filepath = do
  content <- readFile filepath
  pure (parser (Text.pack content))

type ParserMonad s e = StateT s (Either e)

runParserMonad :: ParserMonad s e a -> s -> Either e (a, s)
runParserMonad = State.runStateT
