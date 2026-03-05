{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Parser.Markdown (Error (..), parse) where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline, pattern MkBlock, pattern MkInline)
import Commonmark.Initial qualified as Initial
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State.Class (gets)
import Control.Monad.State.Strict (StateT (..))
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Some (Some (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection, Id)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity, Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Common (drop1)
import Lens.Family2
import Lens.Family2.State.Strict

data Error
  = NoSaveableEntity
  | ParseError Commonmark.ParseError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ParseState s = MkParseState
  { collection :: Collection s
  , maybeURI :: Maybe URI
  , maybeName :: Maybe Name
  , maybeTime :: Maybe Time
  , labels :: [Label]
  , maybeParent :: Maybe (Id s)
  , parents :: [Id s]
  }
  deriving stock (Eq, Show)

empty :: ParseState s
empty =
  MkParseState
    { collection = Collection.empty
    , maybeURI = Nothing
    , maybeName = Nothing
    , maybeTime = Nothing
    , labels = []
    , maybeParent = Nothing
    , parents = []
    }

toEntity :: ParseState s -> Maybe Entity
toEntity st =
  Entity.mkEntity
    <$> st.maybeURI
    <*> st.maybeTime
    <*> pure st.maybeName
    <*> pure (Set.fromList st.labels)

collection :: Lens' (ParseState s) (Collection s)
collection f st = (\c -> st {collection = c}) <$> f st.collection

maybeURI :: Lens' (ParseState s) (Maybe URI)
maybeURI f st = (\u -> st {maybeURI = u}) <$> f st.maybeURI

maybeName :: Lens' (ParseState s) (Maybe Name)
maybeName f st = (\n -> st {maybeName = n}) <$> f st.maybeName

maybeTime :: Lens' (ParseState s) (Maybe Time)
maybeTime f st = (\t -> st {maybeTime = t}) <$> f st.maybeTime

labels :: Lens' (ParseState s) [Label]
labels f st = (\l -> st {labels = l}) <$> f st.labels

maybeParent :: Lens' (ParseState s) (Maybe (Id s))
maybeParent f st = (\p -> st {maybeParent = p}) <$> f st.maybeParent

parents :: Lens' (ParseState s) [Id s]
parents f st = (\p -> st {parents = p}) <$> f st.parents

newtype MarkdownM s a = MkMarkdownM (StateT (ParseState s) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadState (ParseState s), MonadThrow)

runMarkdownM :: MarkdownM s a -> ParseState s -> IO (a, ParseState s)
runMarkdownM (MkMarkdownM m) = runStateT m

liftEither :: (Exception e, HasCallStack) => Either e b -> MarkdownM s b
liftEither = either throwM pure

saveEntity :: MarkdownM s ()
saveEntity = do
  maybeEntity <- gets toEntity
  entity <- maybe (throwM NoSaveableEntity) pure maybeEntity
  (entityId, coll) <- uses collection $ Collection.upsert entity
  collection .= coll
  parent <- uses parents Maybe.listToMaybe
  forM_ parent $ \p ->
    collection %= Collection.addEdges entityId p
  maybeParent .= Just entityId
  maybeURI .= Nothing
  maybeName .= Nothing

textFromInlines :: [Inline a] -> Text
textFromInlines input = LazyText.toStrict (Builder.toLazyText (foldMap go input))
  where
    backtick :: Builder
    backtick = Builder.singleton '`'

    go :: Inline a -> Builder
    go (MkInline _ il) =
      case il of
        Initial.LineBreak -> Builder.singleton '\n'
        Initial.SoftBreak -> Builder.singleton '\SP'
        Initial.Str t -> Builder.fromText t
        Initial.Entity t -> Builder.fromText t
        Initial.EscapedChar c -> Builder.singleton c
        Initial.Emph ils -> foldMap go ils
        Initial.Strong ils -> foldMap go ils
        Initial.Link _ _ ils -> foldMap go ils
        Initial.Image _ _ ils -> foldMap go ils
        Initial.Code t -> backtick <> Builder.fromText t <> backtick
        Initial.RawInline _ t -> Builder.fromText t

extractLink :: Text -> Text -> [Inline a] -> MarkdownM s ()
extractLink dest _title desc = do
  uri <- liftEither (URI.parse dest)
  maybeURI .= Just uri
  let linkText = textFromInlines desc
  when (not (Text.null linkText) && linkText /= dest) $
    maybeName .= Just (MkName linkText)

handleInline :: Inline a -> MarkdownM s ()
handleInline (MkInline _ (Initial.Link dest title desc)) = extractLink dest title desc >> saveEntity
handleInline _ = pure ()

processInlines :: [Inline a] -> MarkdownM s ()
processInlines = mapM_ handleInline

handleBlock :: Block a -> MarkdownM s ()
handleBlock (MkBlock _ b) =
  case b of
    Initial.Plain inlines ->
      processInlines inlines
    Initial.Heading 1 inlines -> do
      let headingText = textFromInlines inlines
      time <- liftEither (Time.parse headingText)
      maybeTime .= Just time
      maybeParent .= Nothing
      labels .= []
    Initial.Heading level inlines -> do
      let headingText = textFromInlines inlines
      let label = MkLabel headingText
      labels %= (label :) . take (level - 2)
    Initial.List _ _ blocksList -> do
      currentParent <- use maybeParent
      forM_ currentParent $ \p ->
        parents %= (p :)
      forM_ blocksList (mapM_ handleBlock)
      maybeParent .= Nothing
      parents %= drop1
    _ -> pure ()

processBlocks :: [Block a] -> MarkdownM s (Collection s)
processBlocks blocks = do
  mapM_ handleBlock blocks
  use collection

parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks = Commonmark.commonmark

parse :: (HasCallStack) => String -> Text -> IO (Some Collection)
parse parseName input = do
  blocks <- either (throwIO . ParseError) pure (parseBlocks parseName input)
  (ret, _) <- runMarkdownM (processBlocks blocks) empty
  pure (Some ret)
