module Hbt.Markdown.Initial.FoldState where

import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Hbt.Collection.Entity (Entity, Label, Name, Time, mkEntity)
import Network.URI (URI)

data FoldState = MkFoldState
  { name :: Last Name
  , time :: Last Time
  , uri :: Last URI
  , labels :: [Label]
  , maybeParent :: Last URI
  , parents :: [URI]
  }

instance Semigroup FoldState where
  a <> b =
    MkFoldState
      (a.name <> b.name)
      (a.time <> b.time)
      (a.uri <> b.uri)
      (a.labels <> b.labels)
      (a.maybeParent <> b.maybeParent)
      (a.parents <> b.parents)

empty :: FoldState
empty =
  MkFoldState
    { name = mempty
    , time = mempty
    , uri = mempty
    , labels = mempty
    , maybeParent = mempty
    , parents = mempty
    }

instance Monoid FoldState where
  mempty = empty

toEntity :: FoldState -> Maybe Entity
toEntity st = case (getLast st.uri, getLast st.time) of
  (Just uri, Just time) -> Just . mkEntity uri time (getLast st.name) $ Set.fromList st.labels
  _ -> Nothing
