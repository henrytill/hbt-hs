{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Attic.Belnap
  ( -- * Scalar type
    Belnap
  , pattern Unknown
  , pattern True
  , pattern False
  , pattern Both

    -- * Scalar operations
  , belnapNot
  , belnapAnd
  , belnapOr
  , merge
  , consensus
  , implies

    -- * Newtype wrappers
  , AsTruth (..)
  , AsKnowledge (..)

    -- * Scalar queries
  , isKnown
  , isDetermined
  , isContradicted
  , toBool

    -- * Error type
  , OutOfBounds (..)

    -- * Bitvector type
  , BelnapVec (..)

    -- * BelnapVec construction
  , mkBelnapVec
  , allTrue
  , allFalse

    -- * BelnapVec scalar access
  , get
  , set

    -- * BelnapVec bulk operations
  , vecNot
  , vecAnd
  , vecOr
  , vecImplies
  , vecMerge
  , vecConsensus

    -- * BelnapVec resize
  , truncate
  , resize

    -- * BelnapVec queries
  , isConsistent
  , isAllDetermined
  , isAllTrue
  , isAllFalse

    -- * BelnapVec counts
  , countTrue
  , countFalse
  , countBoth
  , countUnknown
  )
where

import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Control.Exception (Exception)
import Data.Bits (complement, popCount, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Vector.Unboxed ((!))
import Data.Vector.Unboxed qualified as Unboxed
import Data.Word (Word64, Word8)
import Prelude hiding (False, True, truncate, words)
import Prelude qualified

-- Scalar type

-- | A single Belnap truth value.
--
-- Uses a 'Word8' with the encoding @(neg_bit \`shiftL\` 1) .|. pos_bit@:
--
-- > | pos | neg | bits | variant |
-- > |-----|-----|------|---------|
-- > | 0   | 0   | 0b00 | Unknown |
-- > | 1   | 0   | 0b01 | True    |
-- > | 0   | 1   | 0b10 | False   |
-- > | 1   | 1   | 0b11 | Both    |
newtype Belnap = MkBelnap Word8
  deriving stock (Eq, Ord, Show)

pattern Unknown :: Belnap
pattern Unknown = MkBelnap 0b00

pattern True :: Belnap
pattern True = MkBelnap 0b01

pattern False :: Belnap
pattern False = MkBelnap 0b10

pattern Both :: Belnap
pattern Both = MkBelnap 0b11

{-# COMPLETE Unknown, True, False, Both #-}

-- Scalar operations

-- | Belnap logical NOT: swaps True<->False, leaves Unknown and Both unchanged.
belnapNot :: Belnap -> Belnap
belnapNot (MkBelnap a) =
  MkBelnap $ ((a .&. 1) `shiftL` 1) .|. ((a `shiftR` 1) .&. 1)

-- | Belnap logical AND: pos = pos_a & pos_b, neg = neg_a | neg_b.
belnapAnd :: Belnap -> Belnap -> Belnap
belnapAnd (MkBelnap a) (MkBelnap b) =
  let rPos = (a .&. 1) .&. (b .&. 1)
      rNeg = ((a `shiftR` 1) .&. 1) .|. ((b `shiftR` 1) .&. 1)
   in MkBelnap $ (rNeg `shiftL` 1) .|. rPos

-- | Belnap logical OR: pos = pos_a | pos_b, neg = neg_a & neg_b.
belnapOr :: Belnap -> Belnap -> Belnap
belnapOr (MkBelnap a) (MkBelnap b) =
  let rPos = (a .&. 1) .|. (b .&. 1)
      rNeg = ((a `shiftR` 1) .&. 1) .&. ((b `shiftR` 1) .&. 1)
   in MkBelnap $ (rNeg `shiftL` 1) .|. rPos

-- | Knowledge-ordering join: combine observations from independent sources.
merge :: Belnap -> Belnap -> Belnap
merge (MkBelnap a) (MkBelnap b) = MkBelnap $ a .|. b

-- | Belnap implication: @implies a b = belnapOr (belnapNot a) b@.
implies :: Belnap -> Belnap -> Belnap
implies a b = belnapOr (belnapNot a) b

-- | Knowledge-ordering meet: keep only information that both sources agree on.
consensus :: Belnap -> Belnap -> Belnap
consensus (MkBelnap a) (MkBelnap b) = MkBelnap (a .&. b)

-- Scalar queries

-- | Returns 'Prelude.True' if this value carries any information (not 'Unknown').
isKnown :: Belnap -> Bool
isKnown (MkBelnap a) = a /= 0

-- | Returns 'Prelude.True' if this value is exactly 'True' or 'False'.
isDetermined :: Belnap -> Bool
isDetermined (MkBelnap a) = ((a .&. 1) `xor` ((a `shiftR` 1) .&. 1)) /= 0

-- | Returns 'Prelude.True' if this value is 'Both' (contradicted).
isContradicted :: Belnap -> Bool
isContradicted (MkBelnap a) = a == 0b11

-- | Converts to 'Bool' if the value is exactly 'True' or 'False'.
toBool :: Belnap -> Maybe Bool
toBool True = Just Prelude.True
toBool False = Just Prelude.False
toBool _ = Nothing

-- Newtype wrappers

-- | Wraps a value for the truth-ordering lattice.
-- For 'Belnap': meet = 'belnapAnd', join = 'belnapOr', bottom = 'False', top = 'True'.
-- For 'BelnapVec': meet = 'vecAnd', join = 'vecOr'.
newtype AsTruth a = AsTruth {getTruth :: a}
  deriving stock (Eq, Ord, Show)

instance Lattice (AsTruth Belnap) where
  AsTruth a \/ AsTruth b = AsTruth (belnapOr a b)
  AsTruth a /\ AsTruth b = AsTruth (belnapAnd a b)

instance BoundedJoinSemiLattice (AsTruth Belnap) where
  bottom = AsTruth False

instance BoundedMeetSemiLattice (AsTruth Belnap) where
  top = AsTruth True

instance Lattice (AsTruth BelnapVec) where
  AsTruth a \/ AsTruth b = AsTruth (vecOr a b)
  AsTruth a /\ AsTruth b = AsTruth (vecAnd a b)

-- | Wraps a value for the knowledge-ordering lattice.
-- For 'Belnap': meet = 'consensus', join = 'merge', bottom = 'Unknown', top = 'Both'.
-- For 'BelnapVec': meet = 'vecConsensus', join = 'vecMerge'.
newtype AsKnowledge a = AsKnowledge {getKnowledge :: a}
  deriving stock (Eq, Ord, Show)

instance Lattice (AsKnowledge Belnap) where
  AsKnowledge a \/ AsKnowledge b = AsKnowledge (merge a b)
  AsKnowledge a /\ AsKnowledge b = AsKnowledge (consensus a b)

instance BoundedJoinSemiLattice (AsKnowledge Belnap) where
  bottom = AsKnowledge Unknown

instance BoundedMeetSemiLattice (AsKnowledge Belnap) where
  top = AsKnowledge Both

instance Lattice (AsKnowledge BelnapVec) where
  AsKnowledge a \/ AsKnowledge b = AsKnowledge (vecMerge a b)
  AsKnowledge a /\ AsKnowledge b = AsKnowledge (vecConsensus a b)

instance BoundedJoinSemiLattice (AsKnowledge BelnapVec) where
  bottom = AsKnowledge (mkBelnapVec 0)

-- Error type

data OutOfBounds = OutOfBounds
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- BelnapVec type

-- | Packed Belnap bitvector: two-bitplane interleaved representation.
--
-- Layout: @[pos_0, neg_0, pos_1, neg_1, ...]@ where each word covers 64 positions.
-- Element @i@ lives in word pair @i \`shiftR\` 6@, at bit @i .&. 63@.
-- Unused high bits in the last word pair are always zero.
data BelnapVec = BelnapVec
  { width :: Int
  , words :: Unboxed.Vector Word64
  }
  deriving stock (Eq, Show)

-- Internal helpers

bitsLog2 :: Int
bitsLog2 = 6

bitsMask :: Int
bitsMask = (1 `shiftL` bitsLog2) - 1

wordsNeeded :: Int -> Int
wordsNeeded n = (n + bitsMask) `shiftR` bitsLog2

-- | Mask for valid bits in the last word.  Returns 'maxBound' when @n@ is a
-- multiple of 64 (all bits in the last word are valid).
tailMask :: Int -> Word64
tailMask n =
  let r = n .&. bitsMask
   in if r == 0 then maxBound else (1 `shiftL` r) - 1

-- | Zero out unused high bits in the tail word pair.
--
-- 'tailMask' returns 'maxBound' when @width@ is zero or an exact multiple of
-- 64, meaning every bit in the last word pair is valid and no masking is
-- needed.
maskTail :: BelnapVec -> BelnapVec
maskTail bv
  | m == maxBound = bv
  | otherwise =
      let nw = wordsNeeded bv.width
          base = 2 * (nw - 1)
          words = Unboxed.accum (.&.) bv.words [(base, m), (base + 1, m)]
       in bv {words}
  where
    m = tailMask bv.width

-- | Decompose a 'Belnap' value into its pos-plane and neg-plane fill words.
bitPlanes :: Belnap -> (Word64, Word64)
bitPlanes (MkBelnap bits) =
  ( if bits .&. 1 /= 0 then maxBound else 0
  , if (bits `shiftR` 1) .&. 1 /= 0 then maxBound else 0
  )

-- | Create a vector with every element set to the given value.
filled :: Int -> Belnap -> BelnapVec
filled width fill =
  let nw = wordsNeeded width
      (posW, negW) = bitPlanes fill
      words = Unboxed.generate (2 * nw) (\i -> if even i then posW else negW)
   in maskTail $ BelnapVec {width, words}

-- BelnapVec construction

-- | Create a vector of @width@ elements, all 'Unknown'.
mkBelnapVec :: Int -> BelnapVec
mkBelnapVec width = BelnapVec {width, words = Unboxed.replicate (2 * wordsNeeded width) 0}

-- | Create a vector of @width@ elements, all 'True'.
allTrue :: Int -> BelnapVec
allTrue width = filled width True

-- | Create a vector of @width@ elements, all 'False'.
allFalse :: Int -> BelnapVec
allFalse width = filled width False

-- BelnapVec scalar access

-- | Read element @i@.  Returns 'Left' 'OutOfBounds' if @i >= width@.
get :: Int -> BelnapVec -> Either OutOfBounds Belnap
get i bv
  | i >= bv.width = Left OutOfBounds
  | otherwise =
      let w = i `shiftR` bitsLog2
          b = i .&. bitsMask
          posW = bv.words ! (2 * w)
          negW = bv.words ! (2 * w + 1)
          bitsW = (((negW `shiftR` b) .&. 1) `shiftL` 1) .|. ((posW `shiftR` b) .&. 1) :: Word64
       in Right $ MkBelnap (fromIntegral bitsW)

growTo :: Int -> BelnapVec -> BelnapVec
growTo width bv =
  let oldNw = wordsNeeded bv.width
      newNw = wordsNeeded width
      addition = Unboxed.replicate (2 * (newNw - oldNw)) 0
   in BelnapVec {width, words = Unboxed.concat [bv.words, addition]}

-- | Write element @i@.  Auto-grows the vector (filling with 'Unknown') when
-- @i >= width@, mirroring the Rust @set@ behaviour.
set :: Int -> Belnap -> BelnapVec -> BelnapVec
set i (MkBelnap bits) bv =
  let grown = if i >= bv.width then growTo (i + 1) bv else bv
      w = i `shiftR` bitsLog2
      b = i .&. bitsMask
      bitMask = complement (1 `shiftL` b) :: Word64
      bitsW = fromIntegral bits :: Word64
      posBit = (bitsW .&. 1) `shiftL` b
      negBit = ((bitsW `shiftR` 1) .&. 1) `shiftL` b
      base = 2 * w
      words = Unboxed.accum (\old new -> (old .&. bitMask) .|. new) grown.words [(base, posBit), (base + 1, negBit)]
   in grown {words}

-- BelnapVec bulk operations

-- | Element-wise Belnap NOT.
vecNot :: BelnapVec -> BelnapVec
vecNot bv =
  BelnapVec
    { width = bv.width
    , words = Unboxed.generate n (\i -> bv.words ! (i `xor` 1))
    }
  where
    n = Unboxed.length bv.words

vecBinop ::
  (Word64 -> Word64 -> Word64) ->
  (Word64 -> Word64 -> Word64) ->
  BelnapVec ->
  BelnapVec ->
  BelnapVec
vecBinop posOp negOp a b = BelnapVec {width, words = Unboxed.generate n go}
  where
    width = max a.width b.width
    nw = wordsNeeded width
    n = 2 * nw
    lenA = Unboxed.length a.words
    lenB = Unboxed.length b.words
    getA i = if i < lenA then a.words ! i else 0
    getB i = if i < lenB then b.words ! i else 0
    go i
      | even i = posOp (getA i) (getB i)
      | otherwise = negOp (getA i) (getB i)

-- | Element-wise Belnap AND.  Output width is @max@ of input widths; the
-- shorter vector is extended with 'Unknown'.
vecAnd :: BelnapVec -> BelnapVec -> BelnapVec
vecAnd = vecBinop (.&.) (.|.)

-- | Element-wise Belnap OR.  Output width is @max@ of input widths.
vecOr :: BelnapVec -> BelnapVec -> BelnapVec
vecOr = vecBinop (.|.) (.&.)

-- | Element-wise Belnap implication: @vecImplies a b = vecOr (vecNot a) b@.
vecImplies :: BelnapVec -> BelnapVec -> BelnapVec
vecImplies a b = vecOr (vecNot a) b

-- | Element-wise knowledge-ordering join (merge).  Output width is @max@ of
-- input widths.
vecMerge :: BelnapVec -> BelnapVec -> BelnapVec
vecMerge = vecBinop (.|.) (.|.)

-- | Element-wise knowledge-ordering meet (consensus).  Output width is @max@
-- of input widths.
vecConsensus :: BelnapVec -> BelnapVec -> BelnapVec
vecConsensus = vecBinop (.&.) (.&.)

-- BelnapVec resize

-- | Shrink the vector to @width@ elements.  No-op if @width >= bv.width@.
truncate :: Int -> BelnapVec -> BelnapVec
truncate width bv
  | width >= bv.width = bv
  | otherwise =
      let nw = wordsNeeded width
          words = Unboxed.take (2 * nw) bv.words
       in maskTail $ BelnapVec {width, words}

-- | Resize to @width@ elements, filling new positions with @fill@.
-- Shrinks (discarding elements) when @width <= bv.width@.
resize :: Int -> Belnap -> BelnapVec -> BelnapVec
resize width fill bv
  | width <= bv.width = truncate width bv
  | otherwise =
      let oldNw = wordsNeeded bv.width
          newNw = wordsNeeded width
          (posW, negW) = bitPlanes fill
          highMask = complement (tailMask bv.width)
          base = 2 * (oldNw - 1)
          -- Fill unused high bits in the current last word pair (if any).
          filledWords =
            if isKnown fill && highMask /= 0
              then Unboxed.accum (.|.) bv.words [(base, posW .&. highMask), (base + 1, negW .&. highMask)]
              else bv.words
          -- Append new full word pairs.
          extension =
            Unboxed.generate
              (2 * (newNw - oldNw))
              (\i -> if even i then posW else negW)
          updated = BelnapVec {width, words = Unboxed.concat [filledWords, extension]}
       in if isKnown fill then maskTail updated else updated

-- BelnapVec queries

-- | Returns 'Prelude.True' if no position is 'Both'.
isConsistent :: BelnapVec -> Bool
isConsistent bv =
  all
    (\i -> bv.words ! (2 * i) .&. bv.words ! (2 * i + 1) == 0)
    [0 .. nw - 1]
  where
    nw = wordsNeeded bv.width

-- | Returns 'Prelude.True' if every position is 'True' or 'False'.
isAllDetermined :: BelnapVec -> Bool
isAllDetermined bv =
  all
    ( \i ->
        let mask = if i + 1 == nw then tm else maxBound
         in bv.words ! (2 * i) `xor` bv.words ! (2 * i + 1) == mask
    )
    [0 .. nw - 1]
  where
    nw = wordsNeeded bv.width
    tm = tailMask bv.width

-- | Returns 'Prelude.True' if every position is 'True'.
isAllTrue :: BelnapVec -> Bool
isAllTrue bv =
  all
    ( \i ->
        let mask = if i + 1 == nw then tm else maxBound
         in bv.words ! (2 * i) == mask && bv.words ! (2 * i + 1) == 0
    )
    [0 .. nw - 1]
  where
    nw = wordsNeeded bv.width
    tm = tailMask bv.width

-- | Returns 'Prelude.True' if every position is 'False'.
isAllFalse :: BelnapVec -> Bool
isAllFalse bv =
  all
    ( \i ->
        let mask = if i + 1 == nw then tm else maxBound
         in bv.words ! (2 * i) == 0 && bv.words ! (2 * i + 1) == mask
    )
    [0 .. nw - 1]
  where
    nw = wordsNeeded bv.width
    tm = tailMask bv.width

-- BelnapVec counts

-- | Count positions where the value is 'True'.
countTrue :: BelnapVec -> Int
countTrue bv =
  sum
    [ popCount (bv.words ! (2 * i) .&. complement (bv.words ! (2 * i + 1)))
    | i <- [0 .. nw - 1]
    ]
  where
    nw = wordsNeeded bv.width

-- | Count positions where the value is 'False'.
countFalse :: BelnapVec -> Int
countFalse bv =
  sum
    [ popCount (complement (bv.words ! (2 * i)) .&. bv.words ! (2 * i + 1))
    | i <- [0 .. nw - 1]
    ]
  where
    nw = wordsNeeded bv.width

-- | Count positions where the value is 'Both'.
countBoth :: BelnapVec -> Int
countBoth bv =
  sum
    [ popCount (bv.words ! (2 * i) .&. bv.words ! (2 * i + 1))
    | i <- [0 .. nw - 1]
    ]
  where
    nw = wordsNeeded bv.width

-- | Count positions where the value is 'Unknown'.
countUnknown :: BelnapVec -> Int
countUnknown bv = bv.width - countTrue bv - countFalse bv - countBoth bv
