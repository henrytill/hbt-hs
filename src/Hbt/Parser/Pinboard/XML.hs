{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Monad.Except (Except, MonadError, liftEither, runExcept)
import Control.Monad.State (runStateT)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity)
import Hbt.Parser.Common (lookupAttr)
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

newtype PinboardM a = MkPinboardM (StateT ParseState (Except Error) a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runPinboardM :: PinboardM a -> ParseState -> Either Error (a, ParseState)
runPinboardM (MkPinboardM m) = runExcept . runStateT m

createPostFromAttrs :: [Attribute Text] -> PinboardPost
createPostFromAttrs attrs =
  MkPinboardPost
    { href = Maybe.fromMaybe mempty (lookupAttr "href" attrs)
    , description = Maybe.fromMaybe mempty (lookupAttr "description" attrs)
    , extended = Maybe.fromMaybe mempty (lookupAttr "extended" attrs)
    , time = Maybe.fromMaybe "1970-01-01T00:00:00Z" (lookupAttr "time" attrs)
    , tags = Maybe.fromMaybe mempty (lookupAttr "tag" attrs)
    , shared = if lookupAttr "shared" attrs == Just "yes" then "yes" else "no"
    , toread = if lookupAttr "toread" attrs == Just "yes" then "yes" else "no"
    }

handle :: Tag Text -> PinboardM ()
handle (TagOpen "post" attrs) = do
  let post = createPostFromAttrs attrs
  result <- liftEither $ postToEntity post
  entities %= (result :)
handle _ = return ()

process :: [Tag Text] -> PinboardM Collection
process tags = do
  mapM_ handle tags
  collectedEntities <- use entities
  let finalCollection = foldl' (\coll entity -> snd $ Collection.upsert entity coll) Collection.empty collectedEntities
  collection .= finalCollection
  use collection

parse :: Text -> Either Error Collection
parse input = do
  let tags = TagSoup.parseTags input
  (ret, _) <- runPinboardM (process tags) empty
  return ret

parseFile :: FilePath -> IO (Either Error Collection)
parseFile filepath = do
  content <- readFile filepath
  return $ parse (Text.pack content)
