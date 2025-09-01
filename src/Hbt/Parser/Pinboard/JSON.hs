module Hbt.Parser.Pinboard.JSON where

import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity qualified as Entity
import Hbt.Parser.Common (parseFileWithParser)
import Hbt.Parser.Pinboard.Common (PinboardPost, postToEntity)

data Error
  = EntityInvalidURI String
  | EntityInvalidTime String
  | ParseError String
  deriving (Show, Eq)

fromEntityError :: Entity.Error -> Error
fromEntityError (Entity.InvalidURI s) = EntityInvalidURI s
fromEntityError (Entity.InvalidTime s) = EntityInvalidTime s

postsToCollection :: [PinboardPost] -> Either Error Collection
postsToCollection posts = do
  entities <- traverse (postToEntity fromEntityError) (reverse posts)
  pure (Collection.fromEntities entities)

parse :: Text -> Either Error Collection
parse input = do
  posts <- Bifunctor.first ParseError (Aeson.eitherDecodeStrict (Text.encodeUtf8 input))
  postsToCollection posts

parseFile :: FilePath -> IO (Either Error Collection)
parseFile = parseFileWithParser parse
