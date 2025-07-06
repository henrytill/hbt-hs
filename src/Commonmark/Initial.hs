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

data InlineF r
  = LineBreak
  | SoftBreak
  | Str Text
  | Entity Text
  | EscapedChar Char
  | Emph [r]
  | Strong [r]
  | Link Text Text [r]
  | Image Text Text [r]
  | Code Text
  | RawInline Format Text
  deriving (Show, Eq, Functor, Foldable, Traversable, Data)

$(deriveShow1 ''InlineF)
$(deriveEq1 ''InlineF)

type Inline a = Cofree InlineF a

type Inlines = [Inline Ann]

rangedInline :: SourceRange -> Inline Ann -> Inline Ann
rangedInline sr (ann :< body) = ann <> MkAnn sr mempty :< body

addAttributesInline :: Attributes -> Inline Ann -> Inline Ann
addAttributesInline attrs (ann :< body) = ann <> MkAnn mempty attrs :< body

instance Rangeable Inlines where
  ranged sr = map (rangedInline sr)

instance HasAttributes Inlines where
  addAttributes attrs = map (addAttributesInline attrs)

mkInline :: (Monoid a) => InlineF (Inline a) -> Inline a
mkInline il = mempty :< il

instance IsInline Inlines where
  lineBreak = [mkInline LineBreak]
  softBreak = [mkInline SoftBreak]
  str t = [mkInline (Str t)]
  entity t = [mkInline (Entity t)]
  escapedChar c = [mkInline (EscapedChar c)]
  emph ils = [mkInline (Emph ils)]
  strong ils = [mkInline (Strong ils)]
  link d t desc = [mkInline (Link d t desc)]
  image s t desc = [mkInline (Image s t desc)]
  code t = [mkInline (Code t)]
  rawInline f t = [mkInline (RawInline f t)]

data BlockF a r
  = Paragraph [Inline a]
  | Plain [Inline a]
  | ThematicBreak
  | BlockQuote [r]
  | CodeBlock Text Text
  | Heading Int [Inline a]
  | RawBlock Format Text
  | ReferenceLinkDefinition Text (Text, Text)
  | List ListType ListSpacing [[r]]
  deriving (Show, Eq, Functor, Foldable, Traversable, Data)

$(deriveShow1 ''BlockF)
$(deriveEq1 ''BlockF)

type Block a = Cofree (BlockF a) a

type Blocks = [Block Ann]

rangedBlock :: SourceRange -> Block Ann -> Block Ann
rangedBlock sr (ann :< body) = ann <> MkAnn sr mempty :< body

addAttributesBlock :: Attributes -> Block Ann -> Block Ann
addAttributesBlock attrs (ann :< body) = ann <> MkAnn mempty attrs :< body

instance Rangeable Blocks where
  ranged sr = map (rangedBlock sr)

instance HasAttributes Blocks where
  addAttributes attrs = map (addAttributesBlock attrs)

mkBlock :: (Monoid a) => BlockF a (Block a) -> Block a
mkBlock b = mempty :< b

instance IsBlock Inlines Blocks where
  paragraph ils = [mkBlock (Paragraph ils)]
  plain ils = [mkBlock (Plain ils)]
  thematicBreak = [mkBlock ThematicBreak]
  blockQuote bs = [mkBlock (BlockQuote bs)]
  codeBlock info c = [mkBlock (CodeBlock info c)]
  heading level ils = [mkBlock (Heading level ils)]
  rawBlock f t = [mkBlock (RawBlock f t)]
  referenceLinkDefinition l dt = [mkBlock (ReferenceLinkDefinition l dt)]
  list lt ls bss = [mkBlock (List lt ls bss)]
