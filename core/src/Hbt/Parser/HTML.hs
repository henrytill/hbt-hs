{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.HTML (Error (..), parse) where

import Control.Exception (Exception, throwIO)
import Control.Monad (foldM, forM_, when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (StateT (..))
import Data.Coerce (coerce)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Some (Some (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Common (drop1)
import Lens.Family2
import Lens.Family2.State.Strict
import Text.HTML.Parser (Attr (..), Token (..), parseTokens)

newtype Error = ParseError String
  deriving stock (Eq, Show)
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
  deriving stock (Eq, Show)

data ParseState s = MkParseState
  { collection :: Collection s
  , maybeDescription :: Maybe Text
  , maybeExtended :: Maybe Text
  , attributes :: [Attr]
  , folderStack :: [Text]
  , waitingFor :: WaitingFor
  }
  deriving stock (Eq, Show)

empty :: ParseState s
empty =
  MkParseState
    { collection = Collection.empty
    , maybeDescription = Nothing
    , maybeExtended = Nothing
    , attributes = []
    , folderStack = []
    , waitingFor = None
    }

collection :: Lens' (ParseState s) (Collection s)
collection f st = (\c -> st {collection = c}) <$> f st.collection

maybeDescription :: Lens' (ParseState s) (Maybe Text)
maybeDescription f st = (\d -> st {maybeDescription = d}) <$> f st.maybeDescription

maybeExtended :: Lens' (ParseState s) (Maybe Text)
maybeExtended f st = (\e -> st {maybeExtended = e}) <$> f st.maybeExtended

attributes :: Lens' (ParseState s) [Attr]
attributes f st = (\as -> st {attributes = as}) <$> f st.attributes

folderStack :: Lens' (ParseState s) [Text]
folderStack f st = (\fs -> st {folderStack = fs}) <$> f st.folderStack

waitingFor :: Lens' (ParseState s) WaitingFor
waitingFor f st = (\w -> st {waitingFor = w}) <$> f st.waitingFor

newtype NetscapeM s a = MkNetscapeM (StateT (ParseState s) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadState (ParseState s), MonadIO, MonadThrow)

runNetscapeM :: NetscapeM s a -> ParseState s -> IO (a, ParseState s)
runNetscapeM (MkNetscapeM m) = runStateT m

accumulateEntity :: (HasCallStack) => Entity -> Attr -> IO Entity
accumulateEntity entity (Attr name value) =
  case Text.toLower name of
    "href" -> do
      uri <- either throwIO pure (URI.parse value)
      pure (entity {uri})
    "add_date" ->
      let createdAtTime = Maybe.fromMaybe Time.epoch (Time.parseTimestamp value)
          updatedAt = Set.insert createdAtTime entity.updatedAt
       in pure (entity {updatedAt})
    "last_modified" ->
      let modifiedTime = Time.parseTimestamp value
          updatedAt = maybe entity.updatedAt (`Set.insert` entity.updatedAt) modifiedTime
       in pure (entity {updatedAt})
    "last_visit" ->
      let lastVisitedAtTime = Time.parseTimestamp value
          lastVisitedAt = Entity.MkLastVisitedAt lastVisitedAtTime
       in pure (entity {lastVisitedAt})
    "tags" ->
      let tagList = Text.splitOn "," value
          newLabels = Set.fromList (coerce (filter (/= "toread") tagList))
          toRead = entity.toRead <> if "toread" `elem` tagList then Entity.mkToRead True else mempty
          labels = Set.union entity.labels newLabels
       in pure (entity {labels, toRead})
    "private" -> pure (entity {shared = Entity.mkShared (value /= "1")})
    "toread" -> pure (entity {toRead = Entity.mkToRead (value == "1")})
    "feed" -> pure (entity {isFeed = Entity.mkIsFeed (value == "true")})
    _ -> pure entity

createEntity :: NetscapeM s Entity
createEntity = do
  attrs <- use attributes
  folders <- use folderStack
  name <- use maybeDescription
  ext <- use maybeExtended
  let startEntity = Entity.empty
  accumulated <- liftIO (foldM accumulateEntity startEntity attrs)
  let names = maybe Set.empty (Set.singleton . Entity.MkName) name
      folderLabels = Set.fromList (coerce (reverse folders))
      allLabels = Set.unions [accumulated.labels, folderLabels]
      extended = Maybe.maybeToList (fmap Entity.MkExtended ext)
      finalEntity = accumulated {names, labels = allLabels, extended}
   in if URI.null finalEntity.uri
        then throwM (ParseError "missing required attribute: href")
        else pure finalEntity

addPending :: NetscapeM s ()
addPending = do
  entity <- createEntity
  collection %= snd . Collection.upsert entity
  attributes .= []
  maybeDescription .= Nothing
  maybeExtended .= Nothing

handle :: Token -> NetscapeM s ()
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

process :: [Token] -> NetscapeM s (Collection s)
process tokens = forM_ tokens handle >> use collection

parse :: Text -> IO (Some Collection)
parse input = do
  let tokens = parseTokens input
  (ret, _) <- runNetscapeM (process tokens) empty
  pure (Some ret)
