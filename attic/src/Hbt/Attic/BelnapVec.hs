{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Packed bitvector of Belnap values with bulk operations.
module Hbt.Attic.BelnapVec
  ( -- * Finite type
    Finite
  , getFinite
  , packFinite
  , fin

    -- * Bitvector type
  , BelnapVec

    -- * Construction
  , empty
  , allTrue
  , allFalse
  , allBoth

    -- * Scalar access
  , get
  , set

    -- * Bulk operations
  , not
  , and
  , or
  , implies
  , merge
  , consensus

    -- * Resize
  , resize

    -- * Queries
  , isConsistent
  , isAllDetermined
  , isAllTrue
  , isAllFalse

    -- * Counts
  , countTrue
  , countFalse
  , countBoth
  , countUnknown
  )
where

import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Data.Bits (complement, popCount, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Finite (Finite, getFinite, natToFinite, packFinite)
import Data.Monoid (All (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed.Sized (Vector)
import Data.Vector.Unboxed.Sized qualified as VS
import Data.Word (Word64)
import GHC.Records (HasField (..))
import GHC.TypeLits (Div, KnownNat, Nat, natVal, type (*), type (+), type (<=))
import Hbt.Attic.Belnap (AsKnowledge (..), AsTruth (..), Belnap, unsafeFromBits, unsafeToBits)
import Hbt.Attic.Belnap qualified as Belnap
import Prelude hiding (False, True, and, not, or)

-- | Compile-time index literal: @fin \@i@ produces @Finite n@ when @(i + 1) <= n@.
fin :: forall i n. (KnownNat i, KnownNat n, (i + 1) <= n) => Finite n
fin = natToFinite (Proxy @i)

-- | Storage size in 'Word64' words for a 't:BelnapVec' of @n@ elements.
-- Two bitplanes (pos, neg) interleaved, each needing @ceil(n/64)@ words.
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
bitPlanes (unsafeToBits -> bits) =
  ( if bits .&. 1 /= 0 then maxBound else 0
  , if (bits `shiftR` 1) .&. 1 /= 0 then maxBound else 0
  )

-- | Create a vector with every element set to the given value.
filled :: forall n. (KnownNat n) => Belnap -> BelnapVec n
filled fill =
  let (posW, negW) = bitPlanes fill
   in maskTail . BelnapVec . VS.generate $ \fi ->
        if even (getFinite fi) then posW else negW

-- | Create a vector of @n@ elements, all 'Hbt.Attic.Belnap.Unknown'.
empty :: (KnownNat n) => BelnapVec n
empty = BelnapVec (VS.replicate 0)

-- | Create a vector of @n@ elements, all 'Hbt.Attic.Belnap.True'.
allTrue :: (KnownNat n) => BelnapVec n
allTrue = filled Belnap.True

-- | Create a vector of @n@ elements, all 'Hbt.Attic.Belnap.False'.
allFalse :: (KnownNat n) => BelnapVec n
allFalse = filled Belnap.False

-- | Create a vector of @n@ elements, all 'Hbt.Attic.Belnap.Both'.
allBoth :: (KnownNat n) => BelnapVec n
allBoth = filled Belnap.Both

-- | Read element at index @fi@.
get :: Finite n -> BelnapVec n -> Belnap
get fi (BelnapVec arr) =
  let i = fromIntegral (getFinite fi) :: Int
      w = i `shiftR` bitsLog2
      b = i .&. bitsMask
      posW = VS.unsafeIndex arr (2 * w)
      negW = VS.unsafeIndex arr (2 * w + 1)
      bitsW = (((negW `shiftR` b) .&. 1) `shiftL` 1) .|. ((posW `shiftR` b) .&. 1) :: Word64
   in unsafeFromBits (fromIntegral bitsW)

-- | Write element at index @fi@.
set :: Finite n -> Belnap -> BelnapVec n -> BelnapVec n
set fi (unsafeToBits -> bits) (BelnapVec arr) =
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

-- | Element-wise Belnap NOT.
not :: (KnownNat n) => BelnapVec n -> BelnapVec n
not (BelnapVec arr) =
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
  BelnapVec $ VS.izipWith (\fi x y -> if even (getFinite fi) then posOp x y else negOp x y) a b

-- | Element-wise Belnap AND.
and :: BelnapVec n -> BelnapVec n -> BelnapVec n
and = vecBinop (.&.) (.|.)

-- | Element-wise Belnap OR.
or :: BelnapVec n -> BelnapVec n -> BelnapVec n
or = vecBinop (.|.) (.&.)

-- | Element-wise Belnap implication: @implies a = or (not a)@.
implies :: (KnownNat n) => BelnapVec n -> BelnapVec n -> BelnapVec n
implies a = or (not a)

-- | Element-wise knowledge-ordering join (merge).
merge :: BelnapVec n -> BelnapVec n -> BelnapVec n
merge = vecBinop (.|.) (.|.)

-- | Element-wise knowledge-ordering meet (consensus).
consensus :: BelnapVec n -> BelnapVec n -> BelnapVec n
consensus = vecBinop (.&.) (.&.)

-- | Truncate or extend to @n@ elements ('Hbt.Attic.Belnap.Unknown'-filled), from @m@-wide source.
resize :: forall m n. (KnownNat m, KnownNat n) => BelnapVec m -> BelnapVec n
resize (BelnapVec arr) = maskTail . BelnapVec . VS.generate $ \fi ->
  let i = fromIntegral (getFinite fi) :: Int
      arrWords = VS.length arr
   in if i < arrWords then VS.unsafeIndex arr i else 0

-- | Fold over word pairs, supplying @(mask, pos_word, neg_word)@ to @f@ for
-- each pair.  The mask is 'maxBound' for full words and 'tailMask' for the
-- final partial word; callers that do not need it may ignore it with @_@.
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

-- | Returns 'Prelude.True' if no position is 'Hbt.Attic.Belnap.Both'.
isConsistent :: forall n. (KnownNat n) => BelnapVec n -> Bool
isConsistent = getAll . foldMapWordPairs (\_ pos neg -> All (pos .&. neg == 0))

-- | Returns 'Prelude.True' if every position is 'Hbt.Attic.Belnap.True' or 'Hbt.Attic.Belnap.False'.
isAllDetermined :: forall n. (KnownNat n) => BelnapVec n -> Bool
isAllDetermined = getAll . foldMapWordPairs (\m pos neg -> All (pos `xor` neg == m))

-- | Returns 'Prelude.True' if every position is 'Hbt.Attic.Belnap.True'.
isAllTrue :: forall n. (KnownNat n) => BelnapVec n -> Bool
isAllTrue = getAll . foldMapWordPairs (\m pos neg -> All (pos .&. m == m && neg .&. m == 0))

-- | Returns 'Prelude.True' if every position is 'Hbt.Attic.Belnap.False'.
isAllFalse :: forall n. (KnownNat n) => BelnapVec n -> Bool
isAllFalse = getAll . foldMapWordPairs (\m pos neg -> All (pos .&. m == 0 && neg .&. m == m))

-- | Count positions where the value is 'Hbt.Attic.Belnap.True'.
countTrue :: forall n. (KnownNat n) => BelnapVec n -> Int
countTrue = getSum . foldMapWordPairs (\_ pos neg -> Sum (popCount (pos .&. complement neg)))

-- | Count positions where the value is 'Hbt.Attic.Belnap.False'.
countFalse :: forall n. (KnownNat n) => BelnapVec n -> Int
countFalse = getSum . foldMapWordPairs (\_ pos neg -> Sum (popCount (complement pos .&. neg)))

-- | Count positions where the value is 'Hbt.Attic.Belnap.Both'.
countBoth :: forall n. (KnownNat n) => BelnapVec n -> Int
countBoth = getSum . foldMapWordPairs (\_ pos neg -> Sum (popCount (pos .&. neg)))

-- | Count positions where the value is 'Hbt.Attic.Belnap.Unknown'.
countUnknown :: forall n. (KnownNat n) => BelnapVec n -> Int
countUnknown = getSum . foldMapWordPairs (\m pos neg -> Sum (popCount (complement (pos .|. neg) .&. m)))

instance Lattice (AsTruth (BelnapVec n)) where
  AsTruth a \/ AsTruth b = AsTruth (or a b)
  AsTruth a /\ AsTruth b = AsTruth (and a b)

instance (KnownNat n) => BoundedJoinSemiLattice (AsTruth (BelnapVec n)) where
  bottom = AsTruth allFalse

instance (KnownNat n) => BoundedMeetSemiLattice (AsTruth (BelnapVec n)) where
  top = AsTruth allTrue

instance Lattice (AsKnowledge (BelnapVec n)) where
  AsKnowledge a \/ AsKnowledge b = AsKnowledge (merge a b)
  AsKnowledge a /\ AsKnowledge b = AsKnowledge (consensus a b)

instance (KnownNat n) => BoundedJoinSemiLattice (AsKnowledge (BelnapVec n)) where
  bottom = AsKnowledge empty

instance (KnownNat n) => BoundedMeetSemiLattice (AsKnowledge (BelnapVec n)) where
  top = AsKnowledge allBoth
