{-# LANGUAGE OverloadedStrings #-}

module Hbt.Markdown.Direct where

import Commonmark
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Label (..), Name (..))
import Hbt.Collection.Entity qualified as Entity
import Hbt.Markdown.Direct.FoldState (FoldState (..))
import Hbt.Markdown.Direct.FoldState qualified as FoldState

type Acc = (Collection, FoldState)

newtype Harvester a = MkHarvester (State Acc a)
  deriving (Functor, Applicative, Monad)

get :: Harvester Acc
get = MkHarvester State.get

put :: Acc -> Harvester ()
put = MkHarvester . State.put

modify :: (Acc -> Acc) -> Harvester ()
modify = MkHarvester . State.modify

execHarvester :: Harvester a -> Acc -> Acc
execHarvester (MkHarvester m) = State.execState m

instance Show (Harvester ()) where
  show _ = show ()

instance Semigroup (Harvester ()) where
  (MkHarvester a) <> (MkHarvester b) = MkHarvester $ a >> b

instance Monoid (Harvester ()) where
  mempty = MkHarvester (pure ())

instance Rangeable (Harvester ()) where
  ranged _ m = m

instance HasAttributes (Harvester ()) where
  addAttributes _ m = m

unwrap :: (Show a) => Either a b -> b
unwrap = either (error . show) id

saveEntity :: Harvester ()
saveEntity = modify $ \(c, st) ->
  let e = Maybe.fromJust $ FoldState.toEntity st
      addEdges collection = foldr (\f acc -> unwrap $ Collection.addEdges e.uri f acc) collection (Maybe.listToMaybe st.parents)
   in Collection.upsert e c
        & addEdges
        & (,st {uri = mempty, name = mempty, maybeParent = Last $ Just e.uri})

collectInlineText :: Harvester () -> Harvester Text
collectInlineText harvester = do
  (c0, st0) <- get
  let st1 = FoldState.clearBuffer st0
  put (c0, st1)
  harvester
  (_, st2) <- get
  let collectedText = FoldState.flushBuffer st2
  put (c0, st0)
  return collectedText

instance IsInline (Harvester ()) where
  lineBreak = modify . fmap $ FoldState.appendChar '\n'

  softBreak = modify . fmap $ FoldState.appendChar ' '

  str t = modify . fmap $ FoldState.appendText t

  entity = str

  escapedChar ch = modify . fmap $ FoldState.appendChar ch

  emph ils = ils
  strong ils = ils

  link d _ desc = do
    (c0, st0) <- get
    let st1 = FoldState.clearBuffer st0
    put (c0, st1)
    desc
    (c1, st2) <- get

    case Entity.mkURI (Text.unpack d) of
      Left _ -> put (c1, st0)
      Right uri -> do
        let linkText = FoldState.flushBuffer st2
        let name
              | Text.null linkText || linkText == d = mempty
              | otherwise = Last . Just $ MkName linkText
        put (c1, st0 {name, uri = Last $ Just uri})
        saveEntity
        return ()

  image _ _ desc = desc

  code t = modify . fmap $ FoldState.appendText codeText
    where
      codeText = "`" <> t <> "`"

  rawInline _ = str

instance IsBlock (Harvester ()) (Harvester ()) where
  paragraph ils = ils

  plain ils = ils

  thematicBreak = pure ()

  blockQuote _ = pure ()

  codeBlock _ _ = pure ()

  heading 1 ils = do
    headingText <- collectInlineText ils
    let time = unwrap $ Entity.mkTime (Text.unpack headingText)
    modify . fmap $ \st -> st {time = Last $ Just time, maybeParent = mempty, labels = []}
  heading level ils = do
    headingText <- collectInlineText ils
    modify . fmap $ \st -> st {labels = MkLabel headingText : take (level - 2) st.labels}

  rawBlock _ _ = pure ()

  referenceLinkDefinition _ _ = pure ()

  list _ _ bss = do
    (c0, st0) <- get
    let st1 = foldr (\parent s -> s {parents = parent : s.parents}) st0 st0.maybeParent
    put (c0, st1)
    sequence_ bss
    (c2, _) <- get
    put (c2, st0 {maybeParent = mempty, parents = drop 1 st1.parents})

parse :: String -> Text -> Either ParseError Collection
parse name input = do
  harvester <- commonmark name input :: Either ParseError (Harvester ())
  return . fst $ execHarvester harvester (mempty, mempty)
