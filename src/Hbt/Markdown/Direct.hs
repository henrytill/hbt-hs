{-# LANGUAGE OverloadedStrings #-}

module Hbt.Markdown.Direct where

import Commonmark
import Control.Exception (Exception, throw)
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

data Error = NoSaveableEntity
  deriving (Show, Eq)

instance Exception Error

type Acc = (Collection, FoldState)

newtype Harvester a = MkHarvester (State Acc a)
  deriving (Functor, Applicative, Monad)

instance Show (Harvester ()) where
  show _ = show ()

instance Semigroup (Harvester ()) where
  MkHarvester a <> MkHarvester b = MkHarvester $ a >> b

instance Monoid (Harvester ()) where
  mempty = MkHarvester $ pure ()

instance Rangeable (Harvester ()) where
  ranged _ m = m

instance HasAttributes (Harvester ()) where
  addAttributes _ m = m

get :: Harvester Acc
get = MkHarvester State.get

put :: Acc -> Harvester ()
put = MkHarvester . State.put

modify :: (Acc -> Acc) -> Harvester ()
modify = MkHarvester . State.modify

execHarvester :: Harvester a -> Acc -> Acc
execHarvester (MkHarvester m) = State.execState m

harvestChar :: Char -> Harvester ()
harvestChar c = modify . fmap $ FoldState.appendChar c

harvestText :: Text -> Harvester ()
harvestText t = modify . fmap $ FoldState.appendText t

saveEntity :: Harvester ()
saveEntity = modify $ \(c, st) ->
  let e = Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity st)
      d = Collection.upsert e c
      maybeParent = Last $ Just e.uri
   in foldr (Collection.addEdges e.uri) d (Maybe.listToMaybe st.parents)
        & (,st {maybeParent, uri = mempty, name = mempty})

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
  lineBreak = harvestChar '\n'

  softBreak = harvestChar ' '

  str = harvestText

  entity = harvestText

  escapedChar = harvestChar

  emph ils = ils
  strong ils = ils

  link d _ desc = do
    let uri = Last . Just . Entity.mkURI $ Text.unpack d
    linkText <- collectInlineText desc
    let name
          | Text.null linkText || linkText == d = mempty
          | otherwise = Last . Just $ MkName linkText
    modify . fmap $ \st -> st {uri, name}
    saveEntity
    return ()

  image _ _ desc = desc

  code t = harvestText $ "`" <> t <> "`"

  rawInline _ = harvestText

instance IsBlock (Harvester ()) (Harvester ()) where
  paragraph ils = ils

  plain ils = ils

  thematicBreak = pure ()

  blockQuote _ = pure ()

  codeBlock _ _ = pure ()

  heading 1 ils = do
    headingText <- collectInlineText ils
    let time = Last . Just . Entity.mkTime $ Text.unpack headingText
    modify . fmap $ \st -> st {time, maybeParent = mempty, labels = mempty}
  heading level ils = do
    headingText <- collectInlineText ils
    modify . fmap $ \st -> st {labels = MkLabel headingText : take (level - 2) st.labels}

  rawBlock _ _ = pure ()

  referenceLinkDefinition _ _ = pure ()

  list _ _ bss = do
    (c0, st0) <- get
    let st1 = foldr (\parent st -> st {parents = parent : st.parents}) st0 st0.maybeParent
    put (c0, st1)
    sequence_ bss
    (c2, _) <- get
    put (c2, st0 {maybeParent = mempty, parents = drop 1 st1.parents})

parse :: String -> Text -> Either ParseError Collection
parse name input = do
  harvester <- commonmark name input :: Either ParseError (Harvester ())
  return . fst $ execHarvester harvester mempty
