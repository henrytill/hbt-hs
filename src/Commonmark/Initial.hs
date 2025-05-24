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
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)

data Annotated a b = MkAnnotated
  { annotation :: a
  , value :: b
  }
  deriving (Show, Eq, Functor, Typeable, Data)

data Ann = MkAnn
  { range :: SourceRange
  , attributes :: Attributes
  }
  deriving (Show, Eq, Typeable, Data)

emptyAnn :: Ann
emptyAnn = MkAnn mempty []

data Inline a
  = LineBreak
  | SoftBreak
  | Str Text
  | Entity Text
  | EscapedChar Char
  | Emph a
  | Strong a
  | Link Text Text a
  | Image Text Text a
  | Code Text
  | RawInline Format Text
  deriving (Show, Eq, Functor, Foldable, Traversable, Typeable, Data)

type AnnotatedInline = Annotated Ann (Inline Inlines)

newtype Inlines = MkInlines {unInlines :: [AnnotatedInline]}
  deriving (Show, Eq)

instance Semigroup Inlines where
  MkInlines xs <> MkInlines ys = MkInlines (xs <> ys)

instance Monoid Inlines where
  mempty = MkInlines mempty

instance Rangeable Inlines where
  ranged _ is = is

instance HasAttributes Inlines where
  addAttributes _ is = is

mkInline :: Inline Inlines -> AnnotatedInline
mkInline = MkAnnotated emptyAnn

instance IsInline Inlines where
  lineBreak = MkInlines [mkInline LineBreak]
  softBreak = MkInlines [mkInline SoftBreak]
  str t = MkInlines [mkInline (Str t)]
  entity t = MkInlines [mkInline (Entity t)]
  escapedChar c = MkInlines [mkInline (EscapedChar c)]
  emph ils = MkInlines [mkInline (Emph ils)]
  strong ils = MkInlines [mkInline (Strong ils)]
  link d t desc = MkInlines [mkInline (Link d t desc)]
  image s t desc = MkInlines [mkInline (Image s t desc)]
  code t = MkInlines [mkInline (Code t)]
  rawInline f t = MkInlines [mkInline (RawInline f t)]

data Block il a
  = Paragraph il
  | Plain il
  | ThematicBreak
  | BlockQuote a
  | CodeBlock Text Text
  | Heading Int il
  | RawBlock Format Text
  | ReferenceLinkDefinition Text (Text, Text)
  | List ListType ListSpacing [a]
  deriving (Show, Eq, Functor, Foldable, Traversable, Typeable, Data)

type AnnotatedBlock = Annotated Ann (Block Inlines Blocks)

newtype Blocks = MkBlocks {unBlocks :: [AnnotatedBlock]}
  deriving (Show, Eq)

instance Semigroup Blocks where
  MkBlocks xs <> MkBlocks ys = MkBlocks (xs <> ys)

instance Monoid Blocks where
  mempty = MkBlocks mempty

instance Rangeable Blocks where
  ranged _ bs = bs

instance HasAttributes Blocks where
  addAttributes _ bs = bs

mkBlock :: Block Inlines Blocks -> AnnotatedBlock
mkBlock = MkAnnotated emptyAnn

instance IsBlock Inlines Blocks where
  paragraph ils = MkBlocks [mkBlock (Paragraph ils)]
  plain ils = MkBlocks [mkBlock (Plain ils)]
  thematicBreak = MkBlocks [mkBlock ThematicBreak]
  blockQuote bs = MkBlocks [mkBlock (BlockQuote bs)]
  codeBlock info c = MkBlocks [mkBlock (CodeBlock info c)]
  heading level ils = MkBlocks [mkBlock (Heading level ils)]
  rawBlock f t = MkBlocks [mkBlock (RawBlock f t)]
  referenceLinkDefinition l dt = MkBlocks [mkBlock (ReferenceLinkDefinition l dt)]
  list lt ls bss = MkBlocks [mkBlock (List lt ls bss)]
