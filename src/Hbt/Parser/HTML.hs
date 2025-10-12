{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.HTML where

import Control.Exception (Exception)
import Control.Monad (foldM, forM_, when)
import Control.Monad.Catch (MonadThrow (..))
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Read
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Time)
import Hbt.Entity qualified as Entity
import Hbt.Parser.Common (IsNull (..), ParserMonad, drop1, runParserMonad, pattern Null)
import Lens.Family2
import Lens.Family2.State.Strict
import Text.HTML.Parser (Attr (..), Token (..), parseTokens)

newtype Error = ParseError String
  deriving (Show, Eq)

instance Exception Error

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
  deriving (Show, Eq)

data ParseState = MkParseState
  { collection :: Collection
  , maybeDescription :: Maybe Text
  , maybeExtended :: Maybe Text
  , attributes :: [Attr]
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

attributes :: Lens' ParseState [Attr]
attributes f s = (\as -> s {attributes = as}) <$> f s.attributes

folderStack :: Lens' ParseState [Text]
folderStack f s = (\fs -> s {folderStack = fs}) <$> f s.folderStack

waitingFor :: Lens' ParseState WaitingFor
waitingFor f s = (\w -> s {waitingFor = w}) <$> f s.waitingFor

newtype NetscapeM a = MkNetscapeM (ParserMonad ParseState a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadThrow)

runNetscapeM :: NetscapeM a -> ParseState -> IO (a, ParseState)
runNetscapeM (MkNetscapeM m) = runParserMonad m

parseTimestamp :: Text -> Maybe Time
parseTimestamp str =
  case Read.decimal str of
    Left {} -> Nothing
    Right (timestamp, Null) -> Just (Entity.MkTime (fromInteger timestamp))
    Right {} -> Nothing

accumulateEntity :: Entity -> Attr -> NetscapeM Entity
accumulateEntity entity (Attr name value) =
  case Text.toLower name of
    "href" -> do
      uri <- either (throwM) pure (Entity.mkURI value)
      pure (entity {uri})
    "add_date" -> pure (entity {createdAt = Maybe.fromMaybe (Entity.MkTime 0) (parseTimestamp value)})
    "last_modified" -> pure (entity {updatedAt = Maybe.maybeToList (parseTimestamp value)})
    "last_visit" -> pure (entity {lastVisitedAt = parseTimestamp value})
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
  accumulated <- foldM accumulateEntity startEntity attrs
  let names = maybe Set.empty (Set.singleton . Entity.MkName) name
      folderLabels = Set.fromList (map Entity.MkLabel (reverse folders))
      allLabels = Set.unions [accumulated.labels, folderLabels]
      extended = fmap Entity.MkExtended ext
      finalEntity = accumulated {names, labels = allLabels, extended}
   in if isNull finalEntity.uri
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

parse :: (HasCallStack) => Text -> IO Collection
parse input = do
  let tokens = parseTokens input
  (ret, _) <- runNetscapeM (process tokens) empty
  pure ret
