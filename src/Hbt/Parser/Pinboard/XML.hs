{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Monad.Except (MonadError, liftEither)
import Data.Text (Text)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity)
import Hbt.Parser.Common (ParserMonad, attrMatches, attrOrDefault, attrOrEmpty, parseFileWithParser, requireAttr, runParserMonad)
import Hbt.Parser.Pinboard.Common (Error (..), PinboardPost (..), postToEntity)
import Lens.Family2
import Lens.Family2.State.Lazy
import Text.HTML.TagSoup (Attribute, Tag (..))
import Text.HTML.TagSoup qualified as TagSoup

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

createPostFromAttrs :: [Attribute Text] -> Either Error PinboardPost
createPostFromAttrs attrs = do
  href <- maybe (Left $ MissingRequiredAttribute "href") Right (requireAttr "href" attrs)
  pure $
    MkPinboardPost
      { href
      , description = attrOrEmpty "description" attrs
      , extended = attrOrEmpty "extended" attrs
      , time = attrOrDefault "time" "1970-01-01T00:00:00Z" attrs
      , tags = attrOrEmpty "tag" attrs
      , shared = if attrMatches "shared" "yes" attrs then "yes" else "no"
      , toread = if attrMatches "toread" "yes" attrs then "yes" else "no"
      }

handle :: Tag Text -> PinboardM ()
handle (TagOpen "post" attrs) = do
  post <- liftEither $ createPostFromAttrs attrs
  result <- liftEither $ postToEntity post
  entities %= (result :)
handle _ = return ()

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
  return ret

parseFile :: FilePath -> IO (Either Error Collection)
parseFile = parseFileWithParser parse
