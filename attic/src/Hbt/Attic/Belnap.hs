{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

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

    -- * Scalar queries
  , isKnown
  , isDetermined
  , isContradicted
  , toBool

    -- * Fin type
  , Finite
  , getFinite
  , packFinite
  , fin

    -- * Bitvector type
  , BelnapVec

    -- * BelnapVec construction
  , mkBelnapVec
  , allTrue
  , allFalse
  , allBoth

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

    -- * Newtype wrappers
  , AsTruth (..)
  , AsKnowledge (..)
  )
where

import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Data.Bits (complement, popCount, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Finite (Finite, getFinite, natToFinite, packFinite)
import Data.Monoid (All (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed.Sized (Vector)
import Data.Vector.Unboxed.Sized qualified as VS
import Data.Word (Word64, Word8)
import GHC.Records (HasField (..))
import GHC.TypeLits (Div, KnownNat, Nat, natVal, type (*), type (+), type (<=))
import Prelude hiding (False, True)
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

-- | Belnap implication: @implies a = belnapOr (belnapNot a)@.
implies :: Belnap -> Belnap -> Belnap
implies a = belnapOr (belnapNot a)

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

-- Fin type

-- | Compile-time index literal: @fin \@i@ produces @Finite n@ when @(i + 1) <= n@.
fin :: forall i n. (KnownNat i, KnownNat n, (i + 1) <= n) => Finite n
fin = natToFinite (Proxy @i)

-- BelnapVec type

-- | Storage size in 'Word64' words for a 'BelnapVec' of @n@ elements.
-- Two bitplanes (pos, neg) interleaved, each needing @ceil(n\/64)@ words.
type StorageSize n = 2 * Div (n + 63) 64

-- | Packed Belnap bitvector: two-bitplane interleaved representation.
--
-- Layout: @[pos_0, neg_0, pos_1, neg_1, ...]@ where each word covers 64 positions.
-- Element @i@ lives in word pair @i \`shiftR\` 6@, at bit @i .&. 63@.
-- Unused high bits in the last word pair are always zero.
newtype BelnapVec (n :: Nat) = BelnapVec (Vector (StorageSize n) Word64)
  deriving stock (Eq, Show)

instance (KnownNat n) => HasField "width" (BelnapVec n) Int where
  getField _ = fromIntegral (natVal (Proxy @n))

-- Internal helpers

bitsLog2 :: Int
bitsLog2 = 6

bitsMask :: Int
bitsMask = (1 `shiftL` bitsLog2) - 1

-- | Mask for valid bits in the last word.  Returns 'maxBound' when @n@ is a
-- multiple of 64 (all bits in the last word are valid).
tailMask :: Int -> Word64
tailMask n =
  let r = n .&. bitsMask
   in if r == 0 then maxBound else (1 `shiftL` r) - 1

natInt :: forall n. (KnownNat n) => Int
natInt = fromIntegral (natVal (Proxy @n))

-- | Zero out unused high bits in the tail word pair.
maskTail :: forall n. (KnownNat n) => BelnapVec n -> BelnapVec n
maskTail bv@(BelnapVec arr)
  | m == maxBound = bv
  | otherwise =
      let base = VS.length arr - 2
       in BelnapVec $ VS.unsafeAccum (.&.) arr [(base, m), (base + 1, m)]
  where
    m = tailMask (natInt @n)

-- | Decompose a 'Belnap' value into its pos-plane and neg-plane fill words.
bitPlanes :: Belnap -> (Word64, Word64)
bitPlanes (MkBelnap bits) =
  ( if bits .&. 1 /= 0 then maxBound else 0
  , if (bits `shiftR` 1) .&. 1 /= 0 then maxBound else 0
  )

-- | Create a vector with every element set to the given value.
filled :: forall n. (KnownNat n) => Belnap -> BelnapVec n
filled fill =
  let (posW, negW) = bitPlanes fill
   in maskTail . BelnapVec . VS.generate $ \fi ->
        if even (getFinite fi) then posW else negW

-- BelnapVec construction

-- | Create a vector of @n@ elements, all 'Unknown'.
mkBelnapVec :: (KnownNat n) => BelnapVec n
mkBelnapVec = BelnapVec (VS.replicate 0)

-- | Create a vector of @n@ elements, all 'True'.
allTrue :: (KnownNat n) => BelnapVec n
allTrue = filled True

-- | Create a vector of @n@ elements, all 'False'.
allFalse :: (KnownNat n) => BelnapVec n
allFalse = filled False

-- | Create a vector of @n@ elements, all 'Both'.
allBoth :: (KnownNat n) => BelnapVec n
allBoth = filled Both

-- BelnapVec scalar access

-- | Read element at index @fi@.
get :: Finite n -> BelnapVec n -> Belnap
get fi (BelnapVec arr) =
  let i = fromIntegral (getFinite fi) :: Int
      w = i `shiftR` bitsLog2
      b = i .&. bitsMask
      posW = VS.unsafeIndex arr (2 * w)
      negW = VS.unsafeIndex arr (2 * w + 1)
      bitsW = (((negW `shiftR` b) .&. 1) `shiftL` 1) .|. ((posW `shiftR` b) .&. 1) :: Word64
   in MkBelnap (fromIntegral bitsW)

-- | Write element at index @fi@.
set :: Finite n -> Belnap -> BelnapVec n -> BelnapVec n
set fi (MkBelnap bits) (BelnapVec arr) =
  let i = fromIntegral (getFinite fi) :: Int
      w = i `shiftR` bitsLog2
      b = i .&. bitsMask
      bitMask = complement (1 `shiftL` b) :: Word64
      bitsW = fromIntegral bits :: Word64
      posBit = (bitsW .&. 1) `shiftL` b
      negBit = ((bitsW `shiftR` 1) .&. 1) `shiftL` b
      base = 2 * w
   in BelnapVec $
        VS.unsafeAccum
          (\old new -> (old .&. bitMask) .|. new)
          arr
          [(base, posBit), (base + 1, negBit)]

-- BelnapVec bulk operations

-- | Element-wise Belnap NOT.
vecNot :: (KnownNat n) => BelnapVec n -> BelnapVec n
vecNot (BelnapVec arr) =
  BelnapVec . VS.generate $ \fi ->
    let i = fromIntegral (getFinite fi) :: Int
     in VS.unsafeIndex arr (i `xor` 1)

vecBinop ::
  (Word64 -> Word64 -> Word64) ->
  (Word64 -> Word64 -> Word64) ->
  BelnapVec n ->
  BelnapVec n ->
  BelnapVec n
vecBinop posOp negOp (BelnapVec a) (BelnapVec b) =
  BelnapVec $
    VS.izipWith
      (\fi x y -> if even (getFinite fi) then posOp x y else negOp x y)
      a
      b

-- | Element-wise Belnap AND.
vecAnd :: BelnapVec n -> BelnapVec n -> BelnapVec n
vecAnd = vecBinop (.&.) (.|.)

-- | Element-wise Belnap OR.
vecOr :: BelnapVec n -> BelnapVec n -> BelnapVec n
vecOr = vecBinop (.|.) (.&.)

-- | Element-wise Belnap implication: @vecImplies a = vecOr (vecNot a)@.
vecImplies :: (KnownNat n) => BelnapVec n -> BelnapVec n -> BelnapVec n
vecImplies a = vecOr (vecNot a)

-- | Element-wise knowledge-ordering join (merge).
vecMerge :: BelnapVec n -> BelnapVec n -> BelnapVec n
vecMerge = vecBinop (.|.) (.|.)

-- | Element-wise knowledge-ordering meet (consensus).
vecConsensus :: BelnapVec n -> BelnapVec n -> BelnapVec n
vecConsensus = vecBinop (.&.) (.&.)

-- BelnapVec resize

-- | Truncate or extend to @n@ elements (unknown-filled), from @m@-wide source.
resize :: forall m n. (KnownNat m, KnownNat n) => BelnapVec m -> BelnapVec n
resize (BelnapVec arr) = maskTail . BelnapVec . VS.generate $ \fi ->
  let i = fromIntegral (getFinite fi) :: Int
      arrWords = VS.length arr
   in if i < arrWords then VS.unsafeIndex arr i else 0

-- BelnapVec folds

-- | Fold over word pairs, supplying @(mask, pos_word, neg_word)@ to @f@ for
-- each pair.  The mask is 'maxBound' for full words and 'tailMask' for the
-- final partial word; callers that do not need it may ignore it with @\_@.
foldMapWordPairs ::
  forall n m.
  (KnownNat n, Monoid m) =>
  (Word64 -> Word64 -> Word64 -> m) ->
  BelnapVec n ->
  m
foldMapWordPairs f (BelnapVec arr) =
  foldl'
    ( \acc i ->
        acc
          <> f
            (if i == lastPair then tm else maxBound)
            (VS.unsafeIndex arr i)
            (VS.unsafeIndex arr (i + 1))
    )
    mempty
    [0, 2 .. lastPair]
  where
    lastPair = VS.length arr - 2
    tm = tailMask (natInt @n)

-- BelnapVec queries

-- | Returns 'Prelude.True' if no position is 'Both'.
isConsistent :: forall n. (KnownNat n) => BelnapVec n -> Bool
isConsistent = getAll . foldMapWordPairs (\_ pos neg -> All (pos .&. neg == 0))

-- | Returns 'Prelude.True' if every position is 'True' or 'False'.
isAllDetermined :: forall n. (KnownNat n) => BelnapVec n -> Bool
isAllDetermined = getAll . foldMapWordPairs (\m pos neg -> All (pos `xor` neg == m))

-- | Returns 'Prelude.True' if every position is 'True'.
isAllTrue :: forall n. (KnownNat n) => BelnapVec n -> Bool
isAllTrue = getAll . foldMapWordPairs (\m pos neg -> All (pos .&. m == m && neg .&. m == 0))

-- | Returns 'Prelude.True' if every position is 'False'.
isAllFalse :: forall n. (KnownNat n) => BelnapVec n -> Bool
isAllFalse = getAll . foldMapWordPairs (\m pos neg -> All (pos .&. m == 0 && neg .&. m == m))

-- BelnapVec counts

-- | Count positions where the value is 'True'.
countTrue :: forall n. (KnownNat n) => BelnapVec n -> Int
countTrue = getSum . foldMapWordPairs (\_ pos neg -> Sum (popCount (pos .&. complement neg)))

-- | Count positions where the value is 'False'.
countFalse :: forall n. (KnownNat n) => BelnapVec n -> Int
countFalse = getSum . foldMapWordPairs (\_ pos neg -> Sum (popCount (complement pos .&. neg)))

-- | Count positions where the value is 'Both'.
countBoth :: forall n. (KnownNat n) => BelnapVec n -> Int
countBoth = getSum . foldMapWordPairs (\_ pos neg -> Sum (popCount (pos .&. neg)))

-- | Count positions where the value is 'Unknown'.
countUnknown :: forall n. (KnownNat n) => BelnapVec n -> Int
countUnknown bv = natInt @n - countTrue bv - countFalse bv - countBoth bv

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

instance Lattice (AsTruth (BelnapVec n)) where
  AsTruth a \/ AsTruth b = AsTruth (vecOr a b)
  AsTruth a /\ AsTruth b = AsTruth (vecAnd a b)

instance (KnownNat n) => BoundedJoinSemiLattice (AsTruth (BelnapVec n)) where
  bottom = AsTruth allFalse

instance (KnownNat n) => BoundedMeetSemiLattice (AsTruth (BelnapVec n)) where
  top = AsTruth allTrue

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

instance Lattice (AsKnowledge (BelnapVec n)) where
  AsKnowledge a \/ AsKnowledge b = AsKnowledge (vecMerge a b)
  AsKnowledge a /\ AsKnowledge b = AsKnowledge (vecConsensus a b)

instance (KnownNat n) => BoundedJoinSemiLattice (AsKnowledge (BelnapVec n)) where
  bottom = AsKnowledge mkBelnapVec

instance (KnownNat n) => BoundedMeetSemiLattice (AsKnowledge (BelnapVec n)) where
  top = AsKnowledge allBoth
