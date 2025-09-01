{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Monad.Except (MonadError)
import Control.Monad.Except qualified as Except
import Data.Text (Text)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity)
import Hbt.Collection.Entity qualified as Entity
import Hbt.Parser.Common
import Hbt.Parser.Pinboard.Common
import Lens.Family2
import Lens.Family2.State.Strict
import Text.HTML.TagSoup (Attribute, Tag (..))
import Text.HTML.TagSoup qualified as TagSoup

data Error
  = EntityInvalidURI Text
  | EntityInvalidTime Text
  | ParseError String
  deriving (Show, Eq)

fromEntityError :: Entity.Error -> Error
fromEntityError (Entity.InvalidURI s) = EntityInvalidURI s
fromEntityError (Entity.InvalidTime s) = EntityInvalidTime s

data ParseState = MkParseState
  { collection :: Collection
  , entities :: [Entity]
  }
  deriving (Show, Eq)

empty :: ParseState
empty =
  MkParseState
    { collection = Collection.empty
    , entities = []
    }

collection :: Lens' ParseState Collection
collection f s = (\c -> s {collection = c}) <$> f s.collection

entities :: Lens' ParseState [Entity]
entities f s = (\e -> s {entities = e}) <$> f s.entities

newtype PinboardM a = MkPinboardM (ParserMonad ParseState Error a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runPinboardM :: PinboardM a -> ParseState -> Either Error (a, ParseState)
runPinboardM (MkPinboardM m) = runParserMonad m

accumulatePostAttr :: PinboardPost -> Attribute Text -> PinboardPost
accumulatePostAttr post attr = case attr of
  Href value -> post {href = value}
  Description value -> post {description = value}
  Extended value -> post {extended = value}
  Time value -> post {time = value}
  Tags values -> post {tags = values}
  Shared Yes -> post {shared = "yes"}
  ToRead Yes -> post {toread = "yes"}
  _ -> post

createPostFromAttrs :: [Attribute Text] -> Either Error PinboardPost
createPostFromAttrs attrs = do
  let accumulated = foldl' accumulatePostAttr emptyPost attrs
  if isNull accumulated.href
    then Left (ParseError "missing required attribute: href")
    else pure accumulated

-- It's okay to write point-free code here
handle :: Tag Text -> PinboardM ()
handle (TagOpen "post" attrs) = do
  post <- Except.liftEither (createPostFromAttrs attrs)
  result <- Except.liftEither (postToEntity fromEntityError post)
  entities %= (result :)
handle _ = pure ()

process :: [Tag Text] -> PinboardM Collection
process tags = do
  mapM_ handle tags
  collectedEntities <- use entities
  let finalCollection = Collection.fromEntities collectedEntities
  collection .= finalCollection
  use collection

parse :: Text -> Either Error Collection
parse input = do
  let tags = TagSoup.parseTags input
  (ret, _) <- runPinboardM (process tags) empty
  pure ret

parseFile :: FilePath -> IO (Either Error Collection)
parseFile = parseFileWithParser parse
