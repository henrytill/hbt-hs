{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Markdown.Initial where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline, pattern MkBlock, pattern MkInline)
import Commonmark.Initial qualified as Initial
import Control.Exception (Exception, throw)
import Data.Foldable (foldl')
import Data.Maybe qualified as Maybe
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Label (..), Name (..))
import Hbt.Collection.Entity qualified as Entity
import Hbt.Markdown.Initial.FoldState
import Hbt.Markdown.Initial.FoldState qualified as FoldState
import Lens.Family2
import Lens.Family2.Stock
import Lens.Family2.Unchecked (adapter)

data Error = NoSaveableEntity
  deriving (Show, Eq)

instance Exception Error

type Acc = (Collection, FoldState)

_Last :: Adapter' (Last a) (Maybe a)
_Last = adapter getLast Last

_head :: Getter' [a] (Maybe a)
_head = to Maybe.listToMaybe

saveEntity :: Acc -> Acc
saveEntity acc =
  acc
    & _1 %~ Collection.upsert e
    & _1 %~ maybe id (Collection.addEdges e.uri) (acc ^. _2 . parents . _head)
    & _2 . maybeParent <>~ Last (Just e.uri)
    & _2 . uri .~ mempty
    & _2 . name .~ mempty
  where
    e = Maybe.fromMaybe (throw NoSaveableEntity) (acc ^. _2 . to FoldState.toEntity)

textFromInlines :: [Inline a] -> Text
textFromInlines = LazyText.toStrict . Builder.toLazyText . foldMap go
  where
    go :: Inline a -> Builder
    go (MkInline _ il) = case il of
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

extractLink :: Text -> Text -> [Inline a] -> Acc -> Acc
extractLink d _ desc acc =
  acc
    & _2 . uri <>~ Last updatedURI
    & _2 . name <>~ Last updatedName
  where
    updatedURI = Just . Entity.mkURI $ Text.unpack d
    linkText = textFromInlines desc
    updatedName
      | Text.null linkText || linkText == d = Nothing
      | otherwise = Just $ MkName linkText

inlineFolder :: Acc -> Inline a -> Acc
inlineFolder acc (MkInline _ il) = case il of
  Initial.Link d t desc ->
    acc
      & extractLink d t desc
      & saveEntity
  _ -> acc

blockFolder :: Acc -> Block a -> Acc
blockFolder acc (MkBlock _ b) = case b of
  Initial.Plain ils ->
    foldl' inlineFolder acc ils
  Initial.Heading 1 ils ->
    acc
      & _2 . time <>~ Last updatedTime
      & _2 . maybeParent .~ mempty
      & _2 . labels .~ mempty
    where
      headingText = textFromInlines ils
      updatedTime = Just . Entity.mkTime $ Text.unpack headingText
  Initial.Heading level ils ->
    acc
      & _2 . labels %~ (MkLabel headingText :) . take (level - 2)
    where
      headingText = textFromInlines ils
  Initial.List _ _ bss ->
    acc
      & _2 . parents %~ maybe id (:) (acc ^. _2 . maybeParent . under _Last)
      & foldBlocks bss
      & _2 . maybeParent .~ mempty
      & _2 . parents %~ drop 1
    where
      foldBlocks :: [[Block a]] -> Acc -> Acc
      foldBlocks = flip . foldl' $ foldl' blockFolder
  _ -> acc

collectionFromBlocks :: [Block a] -> Collection
collectionFromBlocks = (^. _1) . foldl' blockFolder mempty

parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks = Commonmark.commonmark

parse :: String -> Text -> Either Commonmark.ParseError Collection
parse parseName input = collectionFromBlocks <$> parseBlocks parseName input
