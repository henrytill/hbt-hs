{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Control.Exception (Exception, throw)
import Data.Maybe qualified as Maybe
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
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
  ( (maybe id (Collection.addEdges eid) (Maybe.listToMaybe s.parents)) c'
  , s {maybeParent = (Just eid), uri = Nothing, name = Nothing}
  )
  where
    (eid, c') = Collection.upsert (Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity s)) c

saveEntity :: Acc -> Acc
saveEntity (c, s) =
  c'
    & maybe id (Collection.addEdges eid) (Maybe.listToMaybe s.parents)
    & (,s {maybeParent = (Just eid), uri = Nothing, name = Nothing})
  where
    e = Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity s)
    (eid, c') = Collection.upsert e c

inspect $ 'saveEntityInline === 'saveEntity

coll :: Lens' Acc Collection
coll = _1

st :: Lens' Acc FoldState
st = _2

saveEntityLens :: Acc -> Acc
saveEntityLens acc =
  acc
    & coll .~ f c
    & st . maybeParent .~ Just eid
    & st . uri .~ Nothing
    & st . name .~ Nothing
  where
    e = acc ^. st . to FoldState.toEntity . to (Maybe.fromMaybe (throw NoSaveableEntity))
    (eid, c) = acc ^. coll & Collection.upsert e
    pid = acc ^. st . parents . to Maybe.listToMaybe
    f = maybe id (Collection.addEdges eid) pid

inspect $ 'saveEntityInline === 'saveEntityLens

main :: IO ()
main = return ()
