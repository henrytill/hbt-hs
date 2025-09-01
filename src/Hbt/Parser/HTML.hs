{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.HTML where

import Control.Monad (forM_, when)
import Control.Monad.Except (MonadError)
import Control.Monad.Except qualified as Except
import Data.Bifunctor qualified as Bifunctor
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Read
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity (..), Time)
import Hbt.Collection.Entity qualified as Entity
import Hbt.Parser.Common (ParserMonad, attrMatches, lookupAttr, parseFileWithParser, requireAttr, runParserMonad, pattern Null)
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

pattern OpenH3 :: [Attribute Text] -> Tag Text
pattern OpenH3 attrs <- TagOpen (Text.toLower -> "h3") attrs

pattern OpenDT :: [Attribute Text] -> Tag Text
pattern OpenDT attrs <- TagOpen (Text.toLower -> "dt") attrs

pattern OpenA :: [Attribute Text] -> Tag Text
pattern OpenA attrs <- TagOpen (Text.toLower -> "a") attrs

pattern OpenDD :: [Attribute Text] -> Tag Text
pattern OpenDD attrs <- TagOpen (Text.toLower -> "dd") attrs

pattern CloseDL :: Tag Text
pattern CloseDL <- TagClose (Text.toLower -> "dl")

data WaitingFor
  = FolderName
  | BookmarkDescription
  | ExtendedDescription
  | None
  deriving (Show, Eq)

data ParseState = MkParseState
  { collection :: Collection
  , maybeDescription :: Maybe Text
  , maybeExtended :: Maybe Text
  , attributes :: [Attribute Text]
  , folderStack :: [Text]
  , waitingFor :: WaitingFor
  }
  deriving (Show, Eq)

empty :: ParseState
empty =
  MkParseState
    { collection = Collection.empty
    , maybeDescription = Nothing
    , maybeExtended = Nothing
    , attributes = []
    , folderStack = []
    , waitingFor = None
    }

collection :: Lens' ParseState Collection
collection f s = (\c -> s {collection = c}) <$> f s.collection

maybeDescription :: Lens' ParseState (Maybe Text)
maybeDescription f s = (\d -> s {maybeDescription = d}) <$> f s.maybeDescription

maybeExtended :: Lens' ParseState (Maybe Text)
maybeExtended f s = (\e -> s {maybeExtended = e}) <$> f s.maybeExtended

attributes :: Lens' ParseState [Attribute Text]
attributes f s = (\as -> s {attributes = as}) <$> f s.attributes

folderStack :: Lens' ParseState [Text]
folderStack f s = (\fs -> s {folderStack = fs}) <$> f s.folderStack

waitingFor :: Lens' ParseState WaitingFor
waitingFor f s = (\w -> s {waitingFor = w}) <$> f s.waitingFor

newtype NetscapeM a = MkNetscapeM (ParserMonad ParseState Error a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runNetscapeM :: NetscapeM a -> ParseState -> Either Error (a, ParseState)
runNetscapeM (MkNetscapeM m) = runParserMonad m

parseTimestamp :: [Attribute Text] -> Text -> Maybe Time
parseTimestamp attrs key =
  case lookupAttr key attrs of
    Nothing -> Nothing
    Just timestampStr ->
      case Read.decimal timestampStr of
        Left {} -> Nothing
        Right (timestamp, Null) -> Just (Entity.MkTime (fromInteger timestamp))
        Right {} -> Nothing

parseTimestampWithDefault :: [Attribute Text] -> Text -> Time
parseTimestampWithDefault attrs key = Maybe.fromMaybe (Entity.MkTime 0) (parseTimestamp attrs key)

parseIsPrivate :: [Attribute Text] -> Bool
parseIsPrivate = attrMatches "private" "1"

parseTagsFromAttr :: [Attribute Text] -> [Text]
parseTagsFromAttr attrs =
  case lookupAttr "tags" attrs of
    Nothing -> []
    Just Null -> []
    Just str -> Text.splitOn "," str

createLabels :: [Text] -> [Text] -> Set Entity.Label
createLabels tags folderLabels =
  let filteredTags = filter (/= "toread") tags
      labelStrings = filteredTags ++ reverse folderLabels
   in Set.fromList (map Entity.MkLabel labelStrings)

createEntity :: [Attribute Text] -> [Text] -> Maybe Text -> Maybe Text -> Either Error Entity
createEntity attrs folders name ext = do
  href <- maybe (Left (ParseError "missing required attribute: href")) Right (requireAttr "href" attrs)
  uri <- Bifunctor.first fromEntityError (Entity.mkURI href)
  let createdAt = parseTimestampWithDefault attrs "add_date"
      updatedAt = Maybe.maybeToList (parseTimestamp attrs "last_modified")
      names = maybe Set.empty (Set.singleton . Entity.MkName) name
      labels = createLabels (parseTagsFromAttr attrs) folders
      shared = not (parseIsPrivate attrs)
      toRead = attrMatches "toread" "1" attrs
      isFeed = attrMatches "feed" "true" attrs
      extended = fmap Entity.MkExtended ext
      lastVisitedAt = parseTimestamp attrs "last_visit"
  pure Entity.MkEntity {uri, createdAt, updatedAt, names, labels, shared, toRead, isFeed, extended, lastVisitedAt}

addPending :: NetscapeM ()
addPending = do
  entity <- Except.liftEither =<< createEntity <$> use attributes <*> use folderStack <*> use maybeDescription <*> use maybeExtended
  collection %= snd . Collection.upsert entity
  attributes .= []
  maybeDescription .= Nothing
  maybeExtended .= Nothing

-- It's okay to write point-free code here
handle :: Tag Text -> NetscapeM ()
handle (OpenH3 _) =
  waitingFor .= FolderName
handle (OpenDT _) = do
  hasAttrs <- uses attributes (not . null)
  when hasAttrs addPending
handle (OpenA attrs) = do
  attributes .= attrs
  waitingFor .= BookmarkDescription
handle (OpenDD _) = do
  hasAttrs <- uses attributes (not . null)
  when hasAttrs $ waitingFor .= ExtendedDescription
handle (TagText str) =
  use waitingFor >>= \w ->
    case w of
      FolderName -> do
        folderStack %= (Text.strip str :)
        waitingFor .= None
      BookmarkDescription -> do
        maybeDescription .= Just (Text.strip str)
        waitingFor .= None
      ExtendedDescription -> do
        maybeExtended .= Just (Text.strip str)
        hasAttrs <- uses attributes (not . null)
        when hasAttrs addPending
        waitingFor .= None
      None -> pure ()
handle CloseDL = do
  hasAttrs <- uses attributes (not . null)
  when hasAttrs addPending
  folderStack %= drop 1
handle _ = pure ()

process :: [Tag Text] -> NetscapeM Collection
process tags = forM_ tags handle >> use collection

parse :: Text -> Either Error Collection
parse input = do
  let tags = TagSoup.parseTags input
  (ret, _) <- runNetscapeM (process tags) empty
  pure ret

parseFile :: FilePath -> IO (Either Error Collection)
parseFile = parseFileWithParser parse
