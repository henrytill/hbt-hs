{-# LANGUAGE TemplateHaskell #-}

module Commonmark.Initial where

import Commonmark.Types
  ( Attributes
  , Format
  , HasAttributes (..)
  , IsBlock (..)
  , IsInline (..)
  , ListSpacing
  , ListType
  , Rangeable (..)
  , SourceRange
  )
import Control.Comonad.Cofree
import Data.Data (Data)
import Data.Eq.Deriving
import Data.Text (Text)
import Text.Show.Deriving

data Ann = MkAnn
  { range :: SourceRange
  , attributes :: Attributes
  }
  deriving (Show, Eq, Data)

instance Semigroup Ann where
  MkAnn a b <> MkAnn c d = MkAnn (a <> c) (b <> d)

instance Monoid Ann where
  mempty = MkAnn mempty mempty

data InlineF a
  = LineBreak
  | SoftBreak
  | Str Text
  | Entity Text
  | EscapedChar Char
  | Emph [a]
  | Strong [a]
  | Link Text Text [a]
  | Image Text Text [a]
  | Code Text
  | RawInline Format Text
  deriving (Show, Eq, Functor, Foldable, Traversable, Data)

$(deriveShow1 ''InlineF)
$(deriveEq1 ''InlineF)

type Inline a = Cofree InlineF a

newtype Inlines = MkInlines {unInlines :: [Inline Ann]}
  deriving (Show, Eq, Data, Semigroup, Monoid)

rangedInline :: SourceRange -> Inline Ann -> Inline Ann
rangedInline sr (ann :< body) = ann <> MkAnn sr mempty :< body

addAttributesInline :: Attributes -> Inline Ann -> Inline Ann
addAttributesInline attrs (ann :< body) = ann <> MkAnn mempty attrs :< body

instance Rangeable Inlines where
  ranged sr (MkInlines ils) = MkInlines (map (rangedInline sr) ils)

instance HasAttributes Inlines where
  addAttributes attrs (MkInlines ils) = MkInlines (map (addAttributesInline attrs) ils)

mkInline :: (Monoid a) => InlineF (Inline a) -> Inline a
mkInline il = mempty :< il

instance IsInline Inlines where
  lineBreak = MkInlines [mkInline LineBreak]
  softBreak = MkInlines [mkInline SoftBreak]
  str t = MkInlines [mkInline (Str t)]
  entity t = MkInlines [mkInline (Entity t)]
  escapedChar c = MkInlines [mkInline (EscapedChar c)]
  emph ils = MkInlines [mkInline (Emph (unInlines ils))]
  strong ils = MkInlines [mkInline (Strong (unInlines ils))]
  link d t desc = MkInlines [mkInline (Link d t (unInlines desc))]
  image s t desc = MkInlines [mkInline (Image s t (unInlines desc))]
  code t = MkInlines [mkInline (Code t)]
  rawInline f t = MkInlines [mkInline (RawInline f t)]

data BlockF a
  = Paragraph Inlines
  | Plain Inlines
  | ThematicBreak
  | BlockQuote [a]
  | CodeBlock Text Text
  | Heading Int Inlines
  | RawBlock Format Text
  | ReferenceLinkDefinition Text (Text, Text)
  | List ListType ListSpacing [[a]]
  deriving (Show, Eq, Functor, Foldable, Traversable, Data)

$(deriveShow1 ''BlockF)
$(deriveEq1 ''BlockF)

type Block a = Cofree BlockF a

newtype Blocks = MkBlocks {unBlocks :: [Block Ann]}
  deriving (Show, Eq, Semigroup, Monoid)

rangedBlock :: SourceRange -> Block Ann -> Block Ann
rangedBlock sr (ann :< body) = ann <> MkAnn sr mempty :< body

addAttributesBlock :: Attributes -> Block Ann -> Block Ann
addAttributesBlock attrs (ann :< body) = ann <> MkAnn mempty attrs :< body

instance Rangeable Blocks where
  ranged sr (MkBlocks bs) = MkBlocks (map (rangedBlock sr) bs)

instance HasAttributes Blocks where
  addAttributes attrs (MkBlocks bs) = MkBlocks (map (addAttributesBlock attrs) bs)

mkBlock :: (Monoid a) => BlockF (Block a) -> Block a
mkBlock b = mempty :< b

instance IsBlock Inlines Blocks where
  paragraph ils = MkBlocks [mkBlock (Paragraph ils)]
  plain ils = MkBlocks [mkBlock (Plain ils)]
  thematicBreak = MkBlocks [mkBlock ThematicBreak]
  blockQuote bs = MkBlocks [mkBlock (BlockQuote (unBlocks bs))]
  codeBlock info c = MkBlocks [mkBlock (CodeBlock info c)]
  heading level ils = MkBlocks [mkBlock (Heading level ils)]
  rawBlock f t = MkBlocks [mkBlock (RawBlock f t)]
  referenceLinkDefinition l dt = MkBlocks [mkBlock (ReferenceLinkDefinition l dt)]
  list lt ls bss = MkBlocks [mkBlock (List lt ls (map unBlocks bss))]
