{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Parser.Markdown where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline, pattern MkBlock, pattern MkInline)
import Commonmark.Initial qualified as Initial
import Control.Monad (forM_, when)
import Control.Monad.Except (MonadError)
import Control.Monad.Except qualified as Except
import Data.Bifunctor qualified as Bifunctor
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
import Hbt.Parser.Common (ParserMonad, parseFileWithParser, runParserMonad)
import Lens.Family2
import Lens.Family2.State.Strict

data Error
  = EntityInvalidURI String
  | EntityInvalidTime String
  | CollectionMissingEntities [Entity.URI]
  | NoSaveableEntity
  | ParseError Commonmark.ParseError
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

newtype MarkdownM a = MkMarkdownM (ParserMonad ParseState Error a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runMarkdownM :: MarkdownM a -> ParseState -> Either Error (a, ParseState)
runMarkdownM (MkMarkdownM m) = runParserMonad m

liftEitherWith :: (a -> Error) -> Either a b -> MarkdownM b
liftEitherWith f ma = Except.liftEither (Bifunctor.first f ma)

saveEntity :: MarkdownM ()
saveEntity = do
  st0 <- use id
  entity <- maybe (Except.throwError NoSaveableEntity) pure (toEntity st0)
  coll0 <- use collection
  let (entityId, coll1) = Collection.upsert entity coll0

  collection .= coll1

  parentStack <- use parents
  forM_ (take 1 parentStack) $ \pid -> do
    coll2 <- use collection
    coll3 <- liftEitherWith fromCollectionError (Collection.addEdges entityId pid coll2)
    collection .= coll3

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
  uri <- liftEitherWith fromEntityError (Entity.mkURI (Text.unpack dest))
  maybeURI .= Just uri

  let linkText = textFromInlines desc
  when (not (Text.null linkText) && linkText /= dest) $
    maybeName .= Just (MkName linkText)

handleInline :: Inline a -> MarkdownM ()
handleInline (MkInline _ (Initial.Link dest title desc)) = extractLink dest title desc >> saveEntity
handleInline _ = pure ()

processInlines :: [Inline a] -> MarkdownM ()
processInlines = mapM_ handleInline

-- It's okay to write point-free code here
handleBlock :: Block a -> MarkdownM ()
handleBlock (MkBlock _ b) =
  case b of
    Initial.Plain inlines ->
      processInlines inlines
    Initial.Heading 1 inlines -> do
      let headingText = textFromInlines inlines
      time <- liftEitherWith fromEntityError (Entity.mkTime (Text.unpack headingText))
      maybeTime .= Just time
      maybeParent .= Nothing
      labels .= []
    Initial.Heading level inlines -> do
      let headingText = textFromInlines inlines
      let label = MkLabel headingText
      labels %= (label :) . take (level - 2)
    Initial.List _ _ blocksList -> do
      currentParent <- use maybeParent
      forM_ currentParent $ \pid -> parents %= (pid :)

      forM_ blocksList $ mapM_ handleBlock

      maybeParent .= Nothing
      parents %= drop 1
    _ -> pure ()

processBlocks :: [Block a] -> MarkdownM Collection
processBlocks blocks = do
  mapM_ handleBlock blocks
  use collection

parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks = Commonmark.commonmark

parse :: String -> Text -> Either Error Collection
parse parseName input = do
  blocks <- Bifunctor.first ParseError (parseBlocks parseName input)
  (ret, _) <- runMarkdownM (processBlocks blocks) empty
  pure ret

parseFile :: FilePath -> IO (Either Error Collection)
parseFile filepath = parseFileWithParser (parse filepath) filepath
