module Hbt.Markdown.Initial.FoldState
  ( FoldState (..)
  , toEntity
  , empty
  , name
  , time
  , uri
  , labels
  , maybeParent
  , parents
  )
where

import Data.Set qualified as Set
import Hbt.Collection (Id)
import Hbt.Collection.Entity (Entity, Label, Name, Time, URI, mkEntity)

data FoldState = MkFoldState
  { name :: Maybe Name
  , time :: Maybe Time
  , uri :: Maybe URI
  , labels :: [Label]
  , maybeParent :: Maybe Id
  , parents :: [Id]
  }

empty :: FoldState
empty =
  MkFoldState
    { name = Nothing
    , time = Nothing
    , uri = Nothing
    , labels = []
    , maybeParent = Nothing
    , parents = []
    }

toEntity :: FoldState -> Maybe Entity
toEntity st = case (st.uri, st.time) of
  (Just u, Just t) -> Just . mkEntity u t st.name $ Set.fromList st.labels
  _ -> Nothing

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

name :: Lens' FoldState (Maybe Name)
name f s = (\n -> s {name = n}) <$> f s.name

time :: Lens' FoldState (Maybe Time)
time f s = (\t -> s {time = t}) <$> f s.time

uri :: Lens' FoldState (Maybe URI)
uri f s = (\u -> s {uri = u}) <$> f s.uri

labels :: Lens' FoldState [Label]
labels f s = (\l -> s {labels = l}) <$> f s.labels

maybeParent :: Lens' FoldState (Maybe Id)
maybeParent f s = (\p -> s {maybeParent = p}) <$> f s.maybeParent

parents :: Lens' FoldState [Id]
parents f s = (\p -> s {parents = p}) <$> f s.parents
