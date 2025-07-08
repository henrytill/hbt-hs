{-# LANGUAGE OverloadedStrings #-}

module Hbt.Markdown where

import Commonmark (ParseError, commonmarkWith, defaultSyntaxSpec)
import Commonmark.Initial (Block, BlockF (..), Blocks, Inline, InlineF (..))
import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Label (..), Name (..), mkTime, mkURI)
import Hbt.Collection.Entity qualified as Entity
import Hbt.Markdown.FoldState (FoldState (..))
import Hbt.Markdown.FoldState qualified as FoldState

data MarkdownError
  = CommonmarkError ParseError
  deriving (Show, Eq)

type Acc = (Collection, FoldState)

saveEntity :: Acc -> Either MarkdownError Acc
saveEntity (c, st) =
  return
    ( foldr (Collection.addEdges entity.uri) (Collection.upsert entity c) (Maybe.listToMaybe st.parents) -- Collection.addEdges throws
    , st {uri = Nothing, name = Nothing, maybeParent = Just entity.uri}
    )
  where
    entity = Maybe.fromJust (FoldState.toEntity st) -- Maybe.fromJust throws

textFromInlines :: [Inline a] -> Text
textFromInlines = foldMap go
  where
    go :: Inline a -> Text
    go (_ :< il) = case il of
      LineBreak -> "\n"
      SoftBreak -> " "
      Str t -> t
      Entity t -> t
      EscapedChar c -> Text.singleton c
      Emph ils -> textFromInlines ils
      Strong ils -> textFromInlines ils
      Link _ _ ils -> textFromInlines ils
      Image _ _ ils -> textFromInlines ils
      Code t -> "`" <> t <> "`"
      RawInline _ t -> t

extractLink :: Text -> Text -> [Inline a] -> Acc -> Either MarkdownError Acc
extractLink d _ desc (c, st) = return (c, st {name, uri})
  where
    uri = Just . mkURI $ Text.unpack d -- mkURI throws
    linkText = textFromInlines desc
    name
      | linkText == d = Nothing
      | Text.null linkText = Nothing
      | otherwise = Just $ MkName linkText

inlineFolder :: Acc -> Inline a -> Either MarkdownError Acc
inlineFolder acc (_ :< il) = case il of
  Link d t desc -> extractLink d t desc acc >>= saveEntity
  _ -> return acc

blockFolder :: Acc -> Block a -> Either MarkdownError Acc
blockFolder acc@(c, st) (_ :< b) = case b of
  Plain ils ->
    foldlM inlineFolder acc ils
  Heading 1 ils ->
    return (c, st {time, maybeParent = Nothing, labels = []})
    where
      headingText = textFromInlines ils
      time = Just . mkTime $ Text.unpack headingText -- mkTime throws
  Heading level ils ->
    return (c, st {labels})
    where
      headingText = textFromInlines ils
      labels = MkLabel headingText : take (level - 2) st.labels
  List _ _ bss ->
    foldlM (foldlM blockFolder) acc' bss <&> fmap f
    where
      acc' = (c, foldr (\parent s -> s {parents = parent : s.parents}) st st.maybeParent)
      f s = s {maybeParent = Nothing, parents = drop 1 s.parents}
  _ -> return acc

collectionFromBlocks :: [Block a] -> Either MarkdownError Collection
collectionFromBlocks bs = foldlM blockFolder (Collection.empty, FoldState.empty) bs <&> fst

parseBlocks :: String -> Text -> Either ParseError Blocks
parseBlocks name = runIdentity . commonmarkWith defaultSyntaxSpec name

parse :: String -> Text -> Either MarkdownError Collection
parse name input = first CommonmarkError (parseBlocks name input) >>= collectionFromBlocks
