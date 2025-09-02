{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.HTML where

import Control.Monad (forM_, when)
import Control.Monad.Except (MonadError)
import Control.Monad.Except qualified as Except
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Read
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity (..), Time)
import Hbt.Collection.Entity qualified as Entity
import Hbt.Parser.Common
import Lens.Family2
import Lens.Family2.State.Strict
import Text.HTML.TagSoup (Attribute, Tag (..))
import Text.HTML.TagSoup qualified as TagSoup

data Error
  = EntityInvalidURI Text
  | EntityInvalidTime Text
  | ParseError String
  deriving (Show, Eq)

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

parseTimestamp :: Text -> Maybe Time
parseTimestamp str =
  case Read.decimal str of
    Left {} -> Nothing
    Right (timestamp, Null) -> Just (Entity.MkTime (fromInteger timestamp))
    Right {} -> Nothing

accumulateEntityAttr :: Entity -> Attribute Text -> Entity
accumulateEntityAttr entity attr =
  case attr of
    Href value -> case Entity.mkURI value of
      Left _ -> entity
      Right uri -> entity {uri}
    AddDate value -> entity {createdAt = Maybe.fromMaybe (Entity.MkTime 0) (parseTimestamp value)}
    LastModified value -> entity {updatedAt = Maybe.maybeToList (parseTimestamp value)}
    LastVisit value -> entity {lastVisitedAt = parseTimestamp value}
    Tags values ->
      let newLabels = Set.fromList (map Entity.MkLabel (filter (/= "toread") values))
          toRead = entity.toRead || "toread" `elem` values
          labels = Set.union entity.labels newLabels
       in entity {labels, toRead}
    Private One -> entity {shared = False}
    ToRead One -> entity {toRead = True}
    Feed STrue -> entity {isFeed = True}
    _ -> entity

createEntity :: [Attribute Text] -> [Text] -> Maybe Text -> Maybe Text -> Either Error Entity
createEntity attrs folders name ext = do
  let startEntity = Entity.empty {shared = True} -- Default to shared in HTML context
      accumulated = foldl' accumulateEntityAttr startEntity attrs
      names = maybe Set.empty (Set.singleton . Entity.MkName) name
      folderLabels = Set.fromList (map Entity.MkLabel (reverse folders))
      allLabels = Set.unions [accumulated.labels, folderLabels]
      extended = fmap Entity.MkExtended ext
      finalEntity = accumulated {names, labels = allLabels, extended}
   in if isNull finalEntity.uri
        then Left (ParseError "missing required attribute: href")
        else pure finalEntity

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
