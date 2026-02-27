{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Attic.Belnap
  ( -- * Scalar type
    Belnap
  , pattern Unknown
  , pattern True
  , pattern False
  , pattern Both

    -- * Unsafe access
  , unsafeToBits
  , unsafeFromBits

    -- * Scalar operations
  , not
  , and
  , or
  , merge
  , consensus
  , implies

    -- * Scalar queries
  , isKnown
  , isDetermined
  , isContradicted
  , toBool

    -- * Newtype wrappers
  , AsTruth (..)
  , AsKnowledge (..)
  )
where

import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word8)
import Prelude hiding (False, True, and, not, or)
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

-- | Extract the raw two-bit encoding. Bit 0 is pos, bit 1 is neg.
unsafeToBits :: Belnap -> Word8
unsafeToBits (MkBelnap w) = w

-- | Construct from a raw two-bit encoding. No bounds checking.
unsafeFromBits :: Word8 -> Belnap
unsafeFromBits = MkBelnap

-- Scalar operations

-- | Belnap logical NOT: swaps True<->False, leaves Unknown and Both unchanged.
not :: Belnap -> Belnap
not (MkBelnap a) =
  MkBelnap $ ((a .&. 1) `shiftL` 1) .|. ((a `shiftR` 1) .&. 1)

-- | Belnap logical AND: pos = pos_a & pos_b, neg = neg_a | neg_b.
and :: Belnap -> Belnap -> Belnap
and (MkBelnap a) (MkBelnap b) =
  let rPos = (a .&. 1) .&. (b .&. 1)
      rNeg = ((a `shiftR` 1) .&. 1) .|. ((b `shiftR` 1) .&. 1)
   in MkBelnap $ (rNeg `shiftL` 1) .|. rPos

-- | Belnap logical OR: pos = pos_a | pos_b, neg = neg_a & neg_b.
or :: Belnap -> Belnap -> Belnap
or (MkBelnap a) (MkBelnap b) =
  let rPos = (a .&. 1) .|. (b .&. 1)
      rNeg = ((a `shiftR` 1) .&. 1) .&. ((b `shiftR` 1) .&. 1)
   in MkBelnap $ (rNeg `shiftL` 1) .|. rPos

-- | Knowledge-ordering join: combine observations from independent sources.
merge :: Belnap -> Belnap -> Belnap
merge (MkBelnap a) (MkBelnap b) = MkBelnap $ a .|. b

-- | Belnap implication: @implies a = or (not a)@.
implies :: Belnap -> Belnap -> Belnap
implies a = or (not a)

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
-- For 'Belnap': meet = 'and', join = 'or', bottom = 'False', top = 'True'.
-- For 'BelnapVec': meet = 'and', join = 'or'.
newtype AsTruth a = AsTruth {getTruth :: a}
  deriving stock (Eq, Ord, Show)

instance Lattice (AsTruth Belnap) where
  AsTruth a \/ AsTruth b = AsTruth (or a b)
  AsTruth a /\ AsTruth b = AsTruth (and a b)

instance BoundedJoinSemiLattice (AsTruth Belnap) where
  bottom = AsTruth False

instance BoundedMeetSemiLattice (AsTruth Belnap) where
  top = AsTruth True

-- | Wraps a value for the knowledge-ordering lattice.
-- For 'Belnap': meet = 'consensus', join = 'merge', bottom = 'Unknown', top = 'Both'.
-- For 'BelnapVec': meet = 'consensus', join = 'merge'.
newtype AsKnowledge a = AsKnowledge {getKnowledge :: a}
  deriving stock (Eq, Ord, Show)

instance Lattice (AsKnowledge Belnap) where
  AsKnowledge a \/ AsKnowledge b = AsKnowledge (merge a b)
  AsKnowledge a /\ AsKnowledge b = AsKnowledge (consensus a b)

instance BoundedJoinSemiLattice (AsKnowledge Belnap) where
  bottom = AsKnowledge Unknown

instance BoundedMeetSemiLattice (AsKnowledge Belnap) where
  top = AsKnowledge Both
