module Hbt.Markdown.Direct.FoldState where

import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Hbt.Collection.Entity (Entity, Label, Name, Time, mkEntity)
import Network.URI (URI)

data FoldState = MkFoldState
  { buffer :: Builder
  , time :: Last Time
  , uri :: Last URI
  , name :: Last Name
  , labels :: [Label]
  , maybeParent :: Last URI
  , parents :: [URI]
  }

instance Semigroup FoldState where
  a <> b =
    MkFoldState
      (a.buffer <> b.buffer)
      (a.time <> b.time)
      (a.uri <> b.uri)
      (a.name <> b.name)
      (a.labels <> b.labels)
      (a.maybeParent <> b.maybeParent)
      (a.parents <> b.parents)

empty :: FoldState
empty =
  MkFoldState
    { buffer = mempty
    , time = mempty
    , uri = mempty
    , name = mempty
    , labels = mempty
    , maybeParent = mempty
    , parents = mempty
    }

instance Monoid FoldState where
  mempty = empty

flushBuffer :: FoldState -> Text
flushBuffer st = LazyText.toStrict . Builder.toLazyText $ st.buffer

clearBuffer :: FoldState -> FoldState
clearBuffer st = st {buffer = mempty}

appendText :: Text -> FoldState -> FoldState
appendText t st = st {buffer = st.buffer <> Builder.fromText t}

appendChar :: Char -> FoldState -> FoldState
appendChar c st = st {buffer = st.buffer <> Builder.singleton c}

toEntity :: FoldState -> Maybe Entity
toEntity st = case (getLast st.uri, getLast st.time) of
  (Just uri, Just time) -> Just . mkEntity uri time (getLast st.name) $ Set.fromList st.labels
  _ -> Nothing
