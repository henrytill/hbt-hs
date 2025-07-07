{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Markdown where

import Commonmark (ParseError, commonmarkWith, defaultSyntaxSpec)
import Commonmark.Initial (Ann (..), Block, BlockF (..), Blocks, Inline, InlineF (..))
import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Foldable (foldl')
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

saveEntity :: Collection -> FoldState -> Acc
saveEntity c st =
  ( foldr addEdges (Collection.upsert entity c) (Maybe.listToMaybe st.parents)
  , st {uri = Nothing, name = Nothing, maybeParent = Just entity.uri}
  )
  where
    entity = Maybe.fromJust (FoldState.toEntity st)
    addEdges parent = Collection.addEdges parent entity.uri

inlinesToText :: [Inline a] -> Text
inlinesToText = foldMap go
  where
    go :: Inline a -> Text
    go (_ :< inline) = case inline of
      LineBreak -> "\n"
      SoftBreak -> " "
      Str t -> t
      Entity t -> t
      EscapedChar c -> Text.singleton c
      Emph inlines -> inlinesToText inlines
      Strong inlines -> inlinesToText inlines
      Link _ _ inlines -> inlinesToText inlines
      Image _ _ inlines -> inlinesToText inlines
      Code t -> "`" <> t <> "`"
      RawInline _ t -> t

handleLink :: Text -> Text -> [Inline a] -> Acc -> Acc
handleLink d _ desc (c, st) = saveEntity c (st {name, uri})
  where
    uri = Just . mkURI $ Text.unpack d
    linkText = inlinesToText desc
    name
      | linkText == d = Nothing
      | Text.null linkText = Nothing
      | otherwise = Just $ MkName linkText

inlineFolder :: Acc -> Inline Ann -> Acc
inlineFolder (c, st) (_ :< il) = case il of
  Link d t desc -> handleLink d t desc (c, st)
  _ -> (c, st)

blockFolder :: Acc -> Block Ann -> Acc
blockFolder (c, st) (_ :< b) = case b of
  Plain ils ->
    foldl' inlineFolder (c, st) ils
  Heading 1 ils ->
    (c, st {time, maybeParent = Nothing, labels = []})
    where
      headingText = inlinesToText ils
      time = Just . mkTime $ Text.unpack headingText
  Heading level ils ->
    (c, st {labels})
    where
      headingText = inlinesToText ils
      labels = MkLabel headingText : take (level - 2) st.labels
  List _ _ bss ->
    f <$> foldl' (foldl' blockFolder) acc bss
    where
      acc = (c, foldr (\parent s -> s {parents = parent : s.parents}) st st.maybeParent)
      f s = s {maybeParent = Nothing, parents = drop 1 $ s.parents}
  _ -> (c, st)

collectionFromBlocks :: Blocks -> Collection
collectionFromBlocks = fst . foldl' blockFolder (Collection.empty, FoldState.empty)

parseBlocks :: String -> Text -> Either ParseError Blocks
parseBlocks name = runIdentity . commonmarkWith defaultSyntaxSpec name

parse :: String -> Text -> Either MarkdownError Collection
parse name input = collectionFromBlocks <$> first CommonmarkError (parseBlocks name input)
