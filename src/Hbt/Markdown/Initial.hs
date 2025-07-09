{-# LANGUAGE OverloadedStrings #-}

module Hbt.Markdown.Initial where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline)
import Commonmark.Initial qualified as Initial
import Control.Comonad.Cofree
import Control.Exception (Exception, throw)
import Data.Foldable (foldl')
import Data.Function ((&))
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
import Hbt.Markdown.Initial.FoldState (FoldState (..))
import Hbt.Markdown.Initial.FoldState qualified as FoldState

data Error = NoSaveableEntity
  deriving (Show, Eq)

instance Exception Error

type Acc = (Collection, FoldState)

saveEntity :: Acc -> Acc
saveEntity (c, st) =
  let e = Maybe.fromMaybe (throw NoSaveableEntity) (FoldState.toEntity st)
      d = Collection.upsert e c
   in foldr (Collection.addEdges e.uri) d (Maybe.listToMaybe st.parents)
        & (,st {uri = mempty, name = mempty, maybeParent = Last $ Just e.uri})

textFromInlines :: [Inline a] -> Text
textFromInlines = LazyText.toStrict . Builder.toLazyText . foldMap go
  where
    go :: Inline a -> Builder
    go (_ :< il) = case il of
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
extractLink d _ desc (c, st) =
  (c, st {name, uri = Last $ Just uri})
  where
    uri = Entity.mkURI $ Text.unpack d
    linkText = textFromInlines desc
    name
      | Text.null linkText || linkText == d = mempty
      | otherwise = Last . Just $ MkName linkText

inlineFolder :: Acc -> Inline a -> Acc
inlineFolder acc (_ :< il) = case il of
  Initial.Link d t desc -> saveEntity $ extractLink d t desc acc
  _ -> acc

blockFolder :: Acc -> Block a -> Acc
blockFolder acc@(c, st) (_ :< b) = case b of
  Initial.Plain ils ->
    foldl' inlineFolder acc ils
  Initial.Heading 1 ils ->
    (c, st {time = Last $ Just time, maybeParent = mempty, labels = []})
    where
      time = Entity.mkTime $ Text.unpack headingText
      headingText = textFromInlines ils
  Initial.Heading level ils ->
    (c, st {labels})
    where
      headingText = textFromInlines ils
      labels = MkLabel headingText : take (level - 2) st.labels
  Initial.List _ _ bss ->
    foldl' (foldl' blockFolder) acc' bss & fmap f
    where
      acc' = (c, foldr (\parent s -> s {parents = parent : s.parents}) st st.maybeParent)
      f s = s {maybeParent = mempty, parents = drop 1 s.parents}
  _ -> acc

collectionFromBlocks :: [Block a] -> Collection
collectionFromBlocks bs = fst $ foldl' blockFolder mempty bs

parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks = Commonmark.commonmark

parse :: String -> Text -> Either Commonmark.ParseError Collection
parse name input = collectionFromBlocks <$> parseBlocks name input
