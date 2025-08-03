module Hbt.Markdown.Initial.FoldState where

import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Hbt.Collection.Entity (Entity, Label, Name, Time, mkEntity)
import Lens.Family2
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
  (Just u, Just t) -> Just . mkEntity u t (getLast st.name) $ Set.fromList st.labels
  _ -> Nothing

name :: Lens' FoldState (Last Name)
name f s = (\n -> s {name = n}) <$> f (s.name)

time :: Lens' FoldState (Last Time)
time f s = (\t -> s {time = t}) <$> f (s.time)

uri :: Lens' FoldState (Last URI)
uri f s = (\u -> s {uri = u}) <$> f (s.uri)

labels :: Lens' FoldState [Label]
labels f s = (\l -> s {labels = l}) <$> f (s.labels)

maybeParent :: Lens' FoldState (Last URI)
maybeParent f s = (\p -> s {maybeParent = p}) <$> f (s.maybeParent)

parents :: Lens' FoldState [URI]
parents f s = (\p -> s {parents = p}) <$> f (s.parents)
