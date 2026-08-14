{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.HTML (Error (..), parse) where

import Control.Exception (Exception, throwIO)
import Control.Monad (foldM, when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (MonadState, StateT (..), execStateT)
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Read qualified as Read
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Common (drop1, uses)
import Lens.Micro
import Lens.Micro.Mtl
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

pattern CloseA :: Token
pattern CloseA <- TagClose (isTagName "a" -> True)

pattern CloseH3 :: Token
pattern CloseH3 <- TagClose (isTagName "h3" -> True)

pattern CloseDL :: Token
pattern CloseDL <- TagClose (isTagName "dl" -> True)

data WaitingFor
  = FolderName
  | BookmarkDescription
  | ExtendedDescription
  | None
  deriving stock (Eq, Show)

data ParseState = MkParseState
  { collection :: Collection
  , maybeDescription :: Maybe Text
  , maybeExtended :: Maybe Text
  , attributes :: [Attr]
  , folderStack :: [Text]
  , waitingFor :: WaitingFor
  , textChunks :: [Text]
  -- ^ Text chunks of the run being read, most recent first.
  }
  deriving stock (Eq)

mkParseState :: Collection -> ParseState
mkParseState coll =
  MkParseState
    { collection = coll
    , maybeDescription = Nothing
    , maybeExtended = Nothing
    , attributes = []
    , folderStack = []
    , waitingFor = None
    , textChunks = []
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

textChunks :: Lens' ParseState [Text]
textChunks f s = (\t -> s {textChunks = t}) <$> f s.textChunks

newtype NetscapeM a = MkNetscapeM (StateT ParseState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadState ParseState, MonadIO, MonadThrow)

execNetscapeM :: NetscapeM a -> ParseState -> IO ParseState
execNetscapeM (MkNetscapeM m) = execStateT m

-- | An entity under construction, alongside the to-read value implied by a
-- @toread@ tag.
--
-- The tag is kept out of the entity so that a TOREAD attribute wins regardless
-- of which of the two comes first in the attribute list: the attribute states
-- the value, the tag only implies it. They are resolved once, in
-- 'resolveToRead', after every attribute has been seen.
type Accum = (Entity, Entity.ToRead)

resolveToRead :: Accum -> Entity
resolveToRead (entity, tagged) = entity {toRead = tagged <> entity.toRead}

-- | Split a TAGS value, discarding surrounding whitespace and empty tags.
splitTags :: Text -> [Text]
splitTags = filter (not . Text.null) . map Text.strip . Text.splitOn ","

-- | The named references a bookmark file's attribute values can carry.
--
-- The lexer decodes references in text content but leaves attribute values
-- exactly as written, so this covers the other half. It is deliberately not
-- the full HTML 5 table: these five are what this formatter and the Go, OCaml
-- and Rust ones emit, and what a URL with a query string needs.
namedRefs :: [(Text, Char)]
namedRefs = [("amp", '&'), ("lt", '<'), ("gt", '>'), ("quot", '"'), ("apos", '\'')]

-- | An upper bound on a reference's length, so a bare @&@ costs a short scan
-- rather than a scan to the end of the value.
maxRefLength :: Int
maxRefLength = 10

decodeRefs :: Text -> Text
decodeRefs input
  | Text.isInfixOf "&" input = LazyText.toStrict (Builder.toLazyText (go input))
  | otherwise = input
  where
    go text
      | Text.null rest = Builder.fromText before
      | otherwise = Builder.fromText before <> decode (Text.drop 1 rest)
      where
        (before, rest) = Text.breakOn "&" text

    decode text
      | not (Text.null semi)
      , Just c <- resolve body =
          Builder.singleton c <> go (Text.drop (Text.length body + 1) text)
      | otherwise = Builder.singleton '&' <> go text
      where
        (body, semi) = Text.breakOn ";" (Text.take maxRefLength text)

    resolve body = case Text.uncons body of
      Just ('#', digits) -> numeric digits
      _ -> lookup body namedRefs

    numeric digits
      | Just hexDigits <- Text.stripPrefix "x" digits = codepoint Read.hexadecimal hexDigits
      | Just hexDigits <- Text.stripPrefix "X" digits = codepoint Read.hexadecimal hexDigits
      | otherwise = codepoint Read.decimal digits

    -- Surrogates are excluded because Text cannot hold them.
    codepoint reader digits = case reader digits of
      Right (n, rest)
        | Text.null rest
        , n > 0
        , n <= 0x10FFFF
        , n < 0xD800 || n > 0xDFFF ->
            Just (Char.chr n)
      _ -> Nothing

accumulateEntity :: (HasCallStack) => Accum -> Attr -> IO Accum
accumulateEntity (entity, tagged) (Attr name rawValue) =
  case Text.toLower name of
    "href" -> do
      uri <- either throwIO pure (URI.parse value)
      keep (entity {uri})
    "add_date" ->
      let createdAtTime = Maybe.fromMaybe Time.epoch (Time.parseTimestamp value)
       in keep (entity {createdAt = Entity.mkCreatedAt createdAtTime})
    "last_modified" ->
      let modifiedTime = Time.parseTimestamp value
          updatedAt = maybe entity.updatedAt (`Set.insert` entity.updatedAt) modifiedTime
       in keep (entity {updatedAt})
    "last_visit" ->
      let lastVisitedAtTime = Time.parseTimestamp value
          lastVisitedAt = Entity.MkLastVisitedAt lastVisitedAtTime
       in keep (entity {lastVisitedAt})
    "tags" ->
      let tagList = splitTags value
          fromTag = if "toread" `elem` tagList then Entity.mkToRead True else mempty
          labels = Set.union entity.labels (Set.fromList (coerce (filter (/= "toread") tagList)))
       in pure (entity {labels}, tagged <> fromTag)
    "private" -> keep (entity {shared = Entity.mkShared (value /= "1")})
    "toread" -> keep (entity {toRead = Entity.mkToRead (value == "1")})
    "feed" -> keep (entity {isFeed = Entity.mkIsFeed (value == "true")})
    _ -> keep entity
  where
    value = decodeRefs rawValue
    keep e = pure (e, tagged)

createEntity :: NetscapeM Entity
createEntity = do
  attrs <- use attributes
  folders <- use folderStack
  name <- use maybeDescription
  ext <- use maybeExtended
  let startEntity = Entity.empty
  accumulated <- liftIO $ resolveToRead <$> foldM accumulateEntity (startEntity, mempty) attrs
  let names = maybe Set.empty (Set.singleton . Entity.MkName) name
      labels = Set.unions [accumulated.labels, Set.fromList . coerce $ reverse folders]
      extended = maybe Set.empty (Set.singleton . Entity.MkExtended) ext
      entity = accumulated {names, labels, extended}
   in if URI.null entity.uri
        then throwM $ ParseError "missing required attribute: href"
        else pure entity

addPending :: NetscapeM ()
addPending = do
  entity <- createEntity
  collection %= snd . Collection.upsert entity
  attributes .= []
  maybeDescription .= Nothing
  maybeExtended .= Nothing

-- | Commit the text run read so far to whatever was waiting for it.
--
-- A run is made of every 'ContentText' token between the tag that started it
-- and the tag that ends it, which is more than one token whenever the text
-- contains a character reference or nested markup: the lexer emits
-- @Tom &amp; Jerry@ as three separate tokens.
flushText :: NetscapeM ()
flushText = do
  what <- use waitingFor
  chunks <- use textChunks
  textChunks .= []
  waitingFor .= None
  let text = Text.strip (Text.concat (reverse chunks))
      maybeText = if Text.null text then Nothing else Just text
  case what of
    -- Pushed even when empty, so that the matching </DL> pops the right folder.
    FolderName -> folderStack %= (text :)
    BookmarkDescription -> maybeDescription .= maybeText
    ExtendedDescription -> maybeExtended .= maybeText
    None -> pure ()

addPendingIfAny :: NetscapeM ()
addPendingIfAny = do
  hasAttrs <- uses attributes (not . null)
  when hasAttrs addPending

handle :: Token -> NetscapeM ()
handle (OpenH3 _) = do
  flushText
  waitingFor .= FolderName
handle CloseH3 =
  flushText
handle (OpenDT _) = do
  flushText
  addPendingIfAny
handle (OpenA attrs) = do
  flushText
  attributes .= attrs
  waitingFor .= BookmarkDescription
handle CloseA =
  flushText
handle (OpenDD _) = do
  flushText
  hasAttrs <- uses attributes (not . null)
  when hasAttrs $ waitingFor .= ExtendedDescription
handle (ContentText str) = do
  reading <- uses waitingFor (/= None)
  when reading $ textChunks %= (str :)
handle CloseDL = do
  flushText
  addPendingIfAny
  folderStack %= drop1
handle _ = pure ()

process :: [Token] -> NetscapeM ()
process tokens = do
  mapM_ handle tokens
  -- Running out of input is not a malformed-input signal, it just means no
  -- more text and no more attributes are coming, so the last bookmark is
  -- recorded here exactly as the other flush points record theirs.
  flushText
  addPendingIfAny

parse :: Text -> IO Collection
parse input = do
  let tokens = parseTokens input
  stateInitial <- mkParseState <$> Collection.new
  stateFinal <- execNetscapeM (process tokens) stateInitial
  pure stateFinal.collection
