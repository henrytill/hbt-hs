{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Parser.Markdown where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline, pattern MkBlock, pattern MkInline)
import Commonmark.Initial qualified as Initial
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadThrow (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection, Id)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity, Label (..), Name (..), Time)
import Hbt.Entity qualified as Entity
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Common (IsEmpty (isEmpty), StateIO, drop1, runStateIO)
import Lens.Family2
import Lens.Family2.State.Strict

data Error
  = NoSaveableEntity
  | ParseError Commonmark.ParseError
  deriving (Show, Eq)

instance Exception Error

data ParseState = MkParseState
  { collection :: Collection
  , maybeURI :: Maybe URI
  , maybeName :: Maybe Name
  , maybeTime :: Maybe Time
  , labels :: [Label]
  , maybeParent :: Maybe Id
  , parents :: [Id]
  }
  deriving (Show, Eq)

empty :: ParseState
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

toEntity :: ParseState -> Maybe Entity
toEntity st = Entity.mkEntity <$> st.maybeURI <*> st.maybeTime <*> pure st.maybeName <*> pure (Set.fromList st.labels)

collection :: Lens' ParseState Collection
collection f s = (\c -> s {collection = c}) <$> f s.collection

maybeURI :: Lens' ParseState (Maybe URI)
maybeURI f s = (\u -> s {maybeURI = u}) <$> f s.maybeURI

maybeName :: Lens' ParseState (Maybe Name)
maybeName f s = (\n -> s {maybeName = n}) <$> f s.maybeName

maybeTime :: Lens' ParseState (Maybe Time)
maybeTime f s = (\t -> s {maybeTime = t}) <$> f s.maybeTime

labels :: Lens' ParseState [Label]
labels f s = (\l -> s {labels = l}) <$> f s.labels

maybeParent :: Lens' ParseState (Maybe Id)
maybeParent f s = (\p -> s {maybeParent = p}) <$> f s.maybeParent

parents :: Lens' ParseState [Id]
parents f s = (\p -> s {parents = p}) <$> f s.parents

newtype MarkdownM a = MkMarkdownM (StateIO ParseState a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadThrow)

runMarkdownM :: MarkdownM a -> ParseState -> IO (a, ParseState)
runMarkdownM (MkMarkdownM m) = runStateIO m

liftEither :: (Exception e) => Either e b -> MarkdownM b
liftEither = either throwM pure

saveEntity :: MarkdownM ()
saveEntity = do
  st0 <- use id
  entity <- maybe (throwM NoSaveableEntity) pure (toEntity st0)
  coll0 <- use collection
  let (entityId, coll1) = Collection.upsert entity coll0
  collection .= coll1
  parentStack <- use parents
  forM_ (take 1 parentStack) $ \pid -> do
    collection %= Collection.addEdges entityId pid
  maybeParent .= Just entityId
  maybeURI .= Nothing
  maybeName .= Nothing

textFromInlines :: [Inline a] -> Text
textFromInlines input = LazyText.toStrict (Builder.toLazyText (foldMap go input))
  where
    go :: Inline a -> Builder
    go (MkInline _ il) =
      case il of
        Initial.LineBreak -> "\n"
        Initial.SoftBreak -> " "
        Initial.Str t -> Builder.fromText t
        Initial.Entity t -> Builder.fromText t
        Initial.EscapedChar c -> Builder.singleton c
        Initial.Emph ils -> foldMap go ils
        Initial.Strong ils -> foldMap go ils
        Initial.Link _ _ ils -> foldMap go ils
        Initial.Image _ _ ils -> foldMap go ils
        Initial.Code t -> "`" <> Builder.fromText t <> "`"
        Initial.RawInline _ t -> Builder.fromText t

extractLink :: Text -> Text -> [Inline a] -> MarkdownM ()
extractLink dest _title desc = do
  uri <- liftEither (URI.parse dest)
  maybeURI .= Just uri
  let linkText = textFromInlines desc
  when (not (isEmpty linkText) && linkText /= dest) $
    maybeName .= Just (MkName linkText)

handleInline :: Inline a -> MarkdownM ()
handleInline (MkInline _ (Initial.Link dest title desc)) = extractLink dest title desc >> saveEntity
handleInline _ = pure ()

processInlines :: [Inline a] -> MarkdownM ()
processInlines = mapM_ handleInline

handleBlock :: Block a -> MarkdownM ()
handleBlock (MkBlock _ b) =
  case b of
    Initial.Plain inlines ->
      processInlines inlines
    Initial.Heading 1 inlines -> do
      let headingText = textFromInlines inlines
      time <- liftEither (Entity.mkTime headingText)
      maybeTime .= Just time
      maybeParent .= Nothing
      labels .= []
    Initial.Heading level inlines -> do
      let headingText = textFromInlines inlines
      let label = MkLabel headingText
      labels %= (label :) . take (level - 2)
    Initial.List _ _ blocksList -> do
      currentParent <- use maybeParent
      forM_ currentParent $ \pid ->
        parents %= (pid :)
      forM_ blocksList (mapM_ handleBlock)
      maybeParent .= Nothing
      parents %= drop1
    _ -> pure ()

processBlocks :: [Block a] -> MarkdownM Collection
processBlocks blocks = do
  mapM_ handleBlock blocks
  use collection

parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks = Commonmark.commonmark

parse :: (HasCallStack) => String -> Text -> IO Collection
parse parseName input = do
  blocks <- either (throwIO . ParseError) pure (parseBlocks parseName input)
  (ret, _) <- runMarkdownM (processBlocks blocks) empty
  pure ret
