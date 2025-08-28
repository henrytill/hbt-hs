{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Parser.Markdown where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline, pattern MkBlock, pattern MkInline)
import Commonmark.Initial qualified as Initial
import Control.Monad (forM_)
import Control.Monad.Except (Except, MonadError, liftEither, runExcept, throwError)
import Control.Monad.State (runStateT)
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Hbt.Collection (Collection, Id)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity, Label (..), Name (..), Time, URI)
import Hbt.Collection.Entity qualified as Entity
import Lens.Family2
import Lens.Family2.State.Lazy

data Error
  = NoSaveableEntity
  | ParseError Commonmark.ParseError
  | EntityInvalidURI String
  | EntityInvalidTime String
  | CollectionMissingEntities [Entity.URI]
  deriving (Show, Eq)

fromEntityError :: Entity.Error -> Error
fromEntityError (Entity.InvalidURI s) = EntityInvalidURI s
fromEntityError (Entity.InvalidTime s) = EntityInvalidTime s

fromCollectionError :: Collection.Error -> Error
fromCollectionError (Collection.MissingEntities uris) = CollectionMissingEntities uris

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

-- Lenses following Netscape pattern
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

newtype MarkdownM a = MkMarkdownM {unMarkdownM :: StateT ParseState (Except Error) a}
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runMarkdownM :: MarkdownM a -> ParseState -> Either Error (a, ParseState)
runMarkdownM (MkMarkdownM m) = runExcept . runStateT m

-- Helper to convert FoldState-like state to Entity
toEntity :: ParseState -> Maybe Entity
toEntity st = case (st.maybeURI, st.maybeTime) of
  (Just u, Just t) -> Just . Entity.mkEntity u t st.maybeName $ Set.fromList st.labels
  _ -> Nothing

-- Save current entity to collection and reset parsing state
saveEntity :: MarkdownM ()
saveEntity = do
  currentState <- use id
  case toEntity currentState of
    Nothing -> throwError NoSaveableEntity
    Just entity -> do
      currentCollection <- use collection
      let (entityId, newCollection) = Collection.upsert entity currentCollection

      -- Update collection
      collection .= newCollection

      -- Add edges if we have a parent from the parent stack (not maybeParent)
      parentStack <- use parents
      case parentStack of
        [] -> return ()
        (pid : _) -> do
          coll <- use collection
          updatedColl <- liftEither . first fromCollectionError $ Collection.addEdges entityId pid coll
          collection .= updatedColl

      -- Set this entity as the new parent and reset parsing fields
      maybeParent .= Just entityId
      maybeURI .= Nothing
      maybeName .= Nothing

-- Extract text from inline elements (same as current implementation)
textFromInlines :: [Inline a] -> Text
textFromInlines = LazyText.toStrict . Builder.toLazyText . foldMap go
  where
    go :: Inline a -> Builder
    go (MkInline _ il) = case il of
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

-- Extract link information and set up for entity creation
extractLink :: Text -> Text -> [Inline a] -> MarkdownM ()
extractLink destination _title description = do
  uri <- liftEither . first fromEntityError . Entity.mkURI $ Text.unpack destination
  maybeURI .= Just uri

  let linkText = textFromInlines description
  if Text.null linkText || linkText == destination
    then maybeName .= Nothing
    else maybeName .= Just (MkName linkText)

-- Handle inline elements
handleInline :: Inline a -> MarkdownM ()
handleInline (MkInline _ il) = case il of
  Initial.Link destination title description -> do
    extractLink destination title description
    saveEntity
  _ -> return ()

-- Process all inlines in a block
processInlines :: [Inline a] -> MarkdownM ()
processInlines = mapM_ handleInline

-- Handle block elements
handleBlock :: Block a -> MarkdownM ()
handleBlock (MkBlock _ b) = case b of
  Initial.Plain inlines ->
    processInlines inlines
  Initial.Heading 1 inlines -> do
    let headingText = textFromInlines inlines
    time <- liftEither . first fromEntityError . Entity.mkTime $ Text.unpack headingText
    maybeTime .= Just time
    maybeParent .= Nothing
    labels .= []
  Initial.Heading level inlines -> do
    let headingText = textFromInlines inlines
    let label = MkLabel headingText
    labels %= (label :) . take (level - 2)
  Initial.List _ _ blocksList -> do
    -- Save current parent and push it to parent stack
    currentParent <- use maybeParent
    case currentParent of
      Nothing -> return ()
      Just pid -> parents %= (pid :)

    -- Process all blocks in the list
    forM_ blocksList $ mapM_ handleBlock

    -- Restore parent state
    maybeParent .= Nothing
    parents %= drop 1
  _ -> return ()

-- Process all blocks
processBlocks :: [Block a] -> MarkdownM Collection
processBlocks blocks = do
  mapM_ handleBlock blocks
  use collection

-- Parse blocks from text
parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks = Commonmark.commonmark

-- Parse markdown text using StateT approach
parse :: String -> Text -> Either Error Collection
parse parseName input = do
  blocks <- liftEither . first ParseError $ parseBlocks parseName input
  fst <$> runMarkdownM (processBlocks blocks) empty

-- Parse from file
parseFile :: FilePath -> IO (Either Error Collection)
parseFile filepath = do
  content <- readFile filepath
  return $ parse filepath (Text.pack content)
