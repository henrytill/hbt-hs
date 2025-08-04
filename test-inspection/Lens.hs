{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Control.Exception (Exception, throw)
import Data.Bifunctor
import Data.Maybe qualified as Maybe
import Data.Monoid (Last (..))
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity qualified as Entity
import Hbt.Markdown.Initial.FoldState
import Hbt.Markdown.Initial.FoldState qualified as FoldState
import Lens.Family2
import Lens.Family2.Stock
import Test.Inspection

data Error = NoSaveableEntity
  deriving (Show, Eq)

instance Exception Error

type Acc = (Collection, FoldState)

saveEntityInline :: Acc -> Acc
saveEntityInline (c, s) =
  ( (maybe id (Collection.addEdges e.uri) (Maybe.listToMaybe s.parents)) (Collection.upsert e c)
  , s {maybeParent = Last (Just e.uri), uri = mempty, name = mempty}
  )
  where
    e = Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity s)

saveEntity :: Acc -> Acc
saveEntity (c, s) =
  Collection.upsert e c
    & maybe id (Collection.addEdges e.uri) (Maybe.listToMaybe s.parents)
    & (,s {maybeParent = Last (Just e.uri), uri = mempty, name = mempty})
  where
    e = Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity s)

coll :: Lens' Acc Collection
coll = _1

st :: Lens' Acc FoldState
st = _2

saveEntityLens :: Acc -> Acc
saveEntityLens acc =
  acc
    & coll %~ Collection.upsert e
    & coll %~ maybe id (Collection.addEdges e.uri) (acc ^. st . parents . to Maybe.listToMaybe)
    & st . maybeParent <>~ Last (Just e.uri)
    & st . uri .~ mempty
    & st . name .~ mempty
  where
    e = Maybe.fromMaybe (throw NoSaveableEntity) (acc ^. st . to FoldState.toEntity)

saveEntityBifunctor :: Acc -> Acc
saveEntityBifunctor acc@(_, s) =
  acc
    & first (Collection.upsert e)
    & first (maybe id (Collection.addEdges e.uri) (Maybe.listToMaybe s.parents))
    & second (\s' -> s' {maybeParent = s'.maybeParent <> Last (Just e.uri)})
    & second (\s' -> s' {uri = mempty})
    & second (\s' -> s' {name = mempty})
  where
    e = Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity s)

inspect $ 'saveEntityInline === 'saveEntity
inspect $ 'saveEntityInline === 'saveEntityLens
inspect $ 'saveEntityInline === 'saveEntityBifunctor

main :: IO ()
main = return ()
