{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.HTML (Error (..), parse) where

import Control.Exception (Exception, throwIO)
import Control.Monad (foldM, forM_, when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Common (StateIO, drop1, runStateIO)
import Lens.Family2
import Lens.Family2.State.Strict
import Text.HTML.Parser (Attr (..), Token (..), parseTokens)

newtype Error = ParseError String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

isTagName :: Text -> Text -> Bool
isTagName expected actual = Text.toLower expected == Text.toLower actual

pattern OpenH3 :: [Attr] -> Token
pattern OpenH3 attrs <- TagOpen (isTagName "h3" -> True) attrs

pattern OpenDT :: [Attr] -> Token
pattern OpenDT attrs <- TagOpen (isTagName "dt" -> True) attrs

pattern OpenA :: [Attr] -> Token
pattern OpenA attrs <- TagOpen (isTagName "a" -> True) attrs

pattern OpenDD :: [Attr] -> Token
pattern OpenDD attrs <- TagOpen (isTagName "dd" -> True) attrs

pattern CloseDL :: Token
pattern CloseDL <- TagClose (isTagName "dl" -> True)

data WaitingFor
  = FolderName
  | BookmarkDescription
  | ExtendedDescription
  | None
  deriving stock (Show, Eq)

data ParseState = MkParseState
  { collection :: Collection
  , maybeDescription :: Maybe Text
  , maybeExtended :: Maybe Text
  , attributes :: [Attr]
  , folderStack :: [Text]
  , waitingFor :: WaitingFor
  }
  deriving stock (Show, Eq)

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

attributes :: Lens' ParseState [Attr]
attributes f s = (\as -> s {attributes = as}) <$> f s.attributes

folderStack :: Lens' ParseState [Text]
folderStack f s = (\fs -> s {folderStack = fs}) <$> f s.folderStack

waitingFor :: Lens' ParseState WaitingFor
waitingFor f s = (\w -> s {waitingFor = w}) <$> f s.waitingFor

newtype NetscapeM a = MkNetscapeM (StateIO ParseState a)
  deriving newtype (Functor, Applicative, Monad, MonadState ParseState, MonadIO, MonadThrow)

runNetscapeM :: NetscapeM a -> ParseState -> IO (a, ParseState)
runNetscapeM (MkNetscapeM m) = runStateIO m

accumulateEntity :: (HasCallStack) => Entity -> Attr -> IO Entity
accumulateEntity entity (Attr name value) =
  case Text.toLower name of
    "href" -> do
      uri <- either throwIO pure (URI.parse value)
      pure (entity {uri})
    "add_date" ->
      let createdAt = Maybe.fromMaybe Time.epoch (Time.parseTimestamp value)
       in pure (entity {createdAt})
    "last_modified" ->
      let updatedAt = Maybe.maybeToList (Time.parseTimestamp value)
       in pure (entity {updatedAt})
    "last_visit" ->
      let lastVisitedAt = Time.parseTimestamp value
       in pure (entity {lastVisitedAt})
    "tags" ->
      let tagList = Text.splitOn "," value
          newLabels = Set.fromList (map Entity.MkLabel (filter (/= "toread") tagList))
          toRead = entity.toRead || "toread" `elem` tagList
          labels = Set.union entity.labels newLabels
       in pure (entity {labels, toRead})
    "private" -> pure (entity {shared = value /= "1"})
    "toread" -> pure (entity {toRead = value == "1"})
    "feed" -> pure (entity {isFeed = value == "true"})
    _ -> pure entity

createEntity :: NetscapeM Entity
createEntity = do
  attrs <- use attributes
  folders <- use folderStack
  name <- use maybeDescription
  ext <- use maybeExtended
  let startEntity = Entity.empty {shared = True}
  accumulated <- liftIO (foldM accumulateEntity startEntity attrs)
  let names = maybe Set.empty (Set.singleton . Entity.MkName) name
      folderLabels = Set.fromList (map Entity.MkLabel (reverse folders))
      allLabels = Set.unions [accumulated.labels, folderLabels]
      extended = fmap Entity.MkExtended ext
      finalEntity = accumulated {names, labels = allLabels, extended}
   in if URI.null finalEntity.uri
        then throwM (ParseError "missing required attribute: href")
        else pure finalEntity

addPending :: NetscapeM ()
addPending = do
  entity <- createEntity
  collection %= snd . Collection.upsert entity
  attributes .= []
  maybeDescription .= Nothing
  maybeExtended .= Nothing

handle :: Token -> NetscapeM ()
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
handle (ContentText str) =
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
  folderStack %= drop1
handle _ = pure ()

process :: [Token] -> NetscapeM Collection
process tokens = forM_ tokens handle >> use collection

parse :: Text -> IO Collection
parse input = do
  let tokens = parseTokens input
  (ret, _) <- runNetscapeM (process tokens) empty
  pure ret
