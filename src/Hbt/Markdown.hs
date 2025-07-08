{-# LANGUAGE OverloadedStrings #-}

module Hbt.Markdown where

import Commonmark qualified
import Commonmark.Initial (Block, Blocks, Inline)
import Commonmark.Initial qualified as Initial
import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Foldable (foldlM, foldrM)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Label (..), Name (..))
import Hbt.Collection.Entity qualified as Entity
import Hbt.Markdown.FoldState (FoldState (..))
import Hbt.Markdown.FoldState qualified as FoldState

data Error
  = CommonmarkError Commonmark.ParseError
  | CollectionError Collection.Error
  | EntityError Entity.Error
  | NoSaveableEntity
  deriving (Show, Eq)

type Acc = (Collection, FoldState)

saveEntity :: Acc -> Either Error Acc
saveEntity (c, st) = do
  entity <- maybe (Left NoSaveableEntity) Right (FoldState.toEntity st)
  let d = Collection.upsert entity c
  e <- first CollectionError $ foldrM (Collection.addEdges entity.uri) d (Maybe.listToMaybe st.parents)
  return (e, st {uri = Nothing, name = Nothing, maybeParent = Just entity.uri})

textFromInlines :: [Inline a] -> Text
textFromInlines = foldMap go
  where
    go :: Inline a -> Text
    go (_ :< il) = case il of
      Initial.LineBreak -> "\n"
      Initial.SoftBreak -> " "
      Initial.Str t -> t
      Initial.Entity t -> t
      Initial.EscapedChar c -> Text.singleton c
      Initial.Emph ils -> textFromInlines ils
      Initial.Strong ils -> textFromInlines ils
      Initial.Link _ _ ils -> textFromInlines ils
      Initial.Image _ _ ils -> textFromInlines ils
      Initial.Code t -> "`" <> t <> "`"
      Initial.RawInline _ t -> t

extractLink :: Text -> Text -> [Inline a] -> Acc -> Either Error Acc
extractLink d _ desc (c, st) = do
  uri <- first EntityError . Entity.mkURI $ Text.unpack d
  return (c, st {name, uri = Just uri})
  where
    linkText = textFromInlines desc
    name
      | Text.null linkText = Nothing
      | linkText == d = Nothing
      | otherwise = Just $ MkName linkText

inlineFolder :: Acc -> Inline a -> Either Error Acc
inlineFolder acc (_ :< il) = case il of
  Initial.Link d t desc -> extractLink d t desc acc >>= saveEntity
  _ -> return acc

blockFolder :: Acc -> Block a -> Either Error Acc
blockFolder acc@(c, st) (_ :< b) = case b of
  Initial.Plain ils ->
    foldlM inlineFolder acc ils
  Initial.Heading 1 ils -> do
    time <- first EntityError . Entity.mkTime $ Text.unpack headingText
    return (c, st {time = Just time, maybeParent = Nothing, labels = []})
    where
      headingText = textFromInlines ils
  Initial.Heading level ils ->
    return (c, st {labels})
    where
      headingText = textFromInlines ils
      labels = MkLabel headingText : take (level - 2) st.labels
  Initial.List _ _ bss ->
    foldlM (foldlM blockFolder) acc' bss <&> fmap f
    where
      acc' = (c, foldr (\parent s -> s {parents = parent : s.parents}) st st.maybeParent)
      f s = s {maybeParent = Nothing, parents = drop 1 s.parents}
  _ -> return acc

collectionFromBlocks :: [Block a] -> Either Error Collection
collectionFromBlocks bs = foldlM blockFolder (Collection.empty, FoldState.empty) bs <&> fst

parseBlocks :: String -> Text -> Either Commonmark.ParseError Blocks
parseBlocks name = runIdentity . Commonmark.commonmarkWith Commonmark.defaultSyntaxSpec name

parse :: String -> Text -> Either Error Collection
parse name input = first CommonmarkError (parseBlocks name input) >>= collectionFromBlocks
