{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hbt.Markdown.FoldState where

import Data.Set qualified as Set
import Hbt.Collection.Entity (Entity, Label, Name, Time, mkEntity)
import Network.URI (URI)

data FoldState = MkFoldState
  { name :: Maybe Name
  , time :: Maybe Time
  , uri :: Maybe URI
  , labels :: [Label]
  , maybeParent :: Maybe URI
  , parents :: [URI]
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
  (Just uri, Just time) -> Just . mkEntity uri time st.name $ Set.fromList st.labels
  _ -> Nothing
