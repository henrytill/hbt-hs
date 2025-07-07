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
  ( foldr addEdges (Collection.upsert entity c) (Maybe.listToMaybe (parents st))
  , st {uri = Nothing, name = Nothing, maybeParent = Just (Entity.uri entity)}
  )
  where
    entity = Maybe.fromJust (FoldState.toEntity st)
    addEdges parent = Collection.addEdges parent (Entity.uri entity)

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
inlineFolder (c, st) il =
  case il of
    (_ :< Link d t desc) -> handleLink d t desc (c, st)
    _ -> (c, st)

blockFolder :: Acc -> Block Ann -> Acc
blockFolder (c, st) b =
  case b of
    (_ :< Plain ils) ->
      foldl' inlineFolder (c, st) ils
    (_ :< Heading 1 ils) ->
      (c, st {time, maybeParent = Nothing, labels = []})
      where
        headingText = inlinesToText ils
        time = Just . mkTime $ Text.unpack headingText
    (_ :< Heading level ils) ->
      (c, st {labels = updatedLabels})
      where
        headingText = inlinesToText ils
        updatedLabels = MkLabel headingText : take (level - 2) (labels st)
    (_ :< List _ _ bss) ->
      f <$> foldl' (foldl' blockFolder) acc bss
      where
        acc = (c, foldr (\parent s -> s {parents = parent : parents s}) st (maybeParent st))
        f s = s {maybeParent = Nothing, parents = drop 1 $ parents s}
    _ -> (c, st)

collectionFromBlocks :: Blocks -> Collection
collectionFromBlocks = fst . foldl' blockFolder (Collection.empty, FoldState.empty)

parseBlocks :: String -> Text -> Either ParseError Blocks
parseBlocks name = runIdentity . commonmarkWith defaultSyntaxSpec name

parse :: String -> Text -> Either MarkdownError Collection
parse name input = collectionFromBlocks <$> first CommonmarkError (parseBlocks name input)
