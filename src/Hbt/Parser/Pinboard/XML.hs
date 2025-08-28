{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Monad.Except (Except, MonadError, liftEither, runExcept)
import Control.Monad.State (runStateT)
import Data.Bifunctor (first)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity, Extended (..), Label (..), Name (..), Time (..))
import Hbt.Collection.Entity qualified as Entity
import Lens.Family2
import Lens.Family2.State.Lazy
import Text.HTML.TagSoup (Attribute, Tag (..))
import Text.HTML.TagSoup qualified as TagSoup

data Error
  = EntityInvalidURI String
  | EntityInvalidTime String
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

newtype PinboardM a = MkPinboardM {unPinboardM :: StateT ParseState (Except Error) a}
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runPinboardM :: PinboardM a -> ParseState -> Either Error (a, ParseState)
runPinboardM (MkPinboardM m) = runExcept . runStateT m

lookupAttr :: Text -> [Attribute Text] -> Maybe Text
lookupAttr key attrs = lookup (Text.toLower key) (map (\(k, v) -> (Text.toLower k, v)) attrs)

parseTime :: Text -> Either Error Time
parseTime timeStr =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack timeStr) :: Maybe UTCTime of
    Just utcTime -> Right $ MkTime (utcTimeToPOSIXSeconds utcTime)
    Nothing -> Left $ EntityInvalidTime (Text.unpack timeStr)

parseTags :: Maybe Text -> [Label]
parseTags Nothing = []
parseTags (Just tagStr) =
  map (MkLabel . Text.strip) $
    filter (not . Text.null) $
      Text.splitOn " " tagStr

createEntity :: [Attribute Text] -> Either Error Entity
createEntity attrs = do
  uri <- first fromEntityError $ Entity.mkURI . Text.unpack $ Maybe.fromMaybe mempty (lookupAttr "href" attrs)
  createdAt <- parseTime $ Maybe.fromMaybe "1970-01-01T00:00:00Z" (lookupAttr "time" attrs)

  let description = lookupAttr "description" attrs
      name =
        if Maybe.isJust description && not (Text.null (Maybe.fromMaybe "" description))
          then MkName <$> description
          else Nothing
      labels = Set.fromList $ parseTags (lookupAttr "tag" attrs)
      extended =
        if Maybe.isJust (lookupAttr "extended" attrs) && not (Text.null (Maybe.fromMaybe "" (lookupAttr "extended" attrs)))
          then MkExtended <$> lookupAttr "extended" attrs
          else Nothing
      shared = lookupAttr "shared" attrs == Just "yes"
      toRead = lookupAttr "toread" attrs == Just "yes"

  pure $
    (Entity.mkEntity uri createdAt name labels)
      { Entity.extended = extended
      , Entity.shared = shared
      , Entity.toRead = toRead
      , Entity.isFeed = False
      }

handle :: Tag Text -> PinboardM ()
handle (TagOpen "post" attrs) = do
  result <- liftEither $ createEntity attrs
  entities %= (result :)
handle _ = return ()

process :: [Tag Text] -> PinboardM Collection
process tags = do
  mapM_ handle tags
  collectedEntities <- use entities
  let finalCollection = foldl (\coll entity -> snd $ Collection.upsert entity coll) Collection.empty collectedEntities
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
