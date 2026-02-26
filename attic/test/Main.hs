{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)
import Hbt.Attic.Belnap (AsKnowledge (..), AsTruth (..), Belnap, BelnapVec)
import Hbt.Attic.Belnap qualified as Belnap
import System.Exit (exitFailure)
import Test.Dwergaz
import Test.QuickCheck (Arbitrary (..), Gen, Property, elements, forAll, isSuccess, quickCheckResult, vectorOf, (===))

variants :: [Belnap]
variants = [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]

-- Helper: construct a Finite n from an Int (panics if out of bounds)
fi :: (KnownNat n) => Int -> Belnap.Finite n
fi = fromJust . Belnap.packFinite . fromIntegral

-- Scalar tests

scalarNotTests :: Test
scalarNotTests =
  group
    "scalar not"
    [ assertEqual "not True = False" Belnap.False (Belnap.belnapNot Belnap.True)
    , assertEqual "not False = True" Belnap.True (Belnap.belnapNot Belnap.False)
    , assertEqual "not Unknown = Unknown" Belnap.Unknown (Belnap.belnapNot Belnap.Unknown)
    , assertEqual "not Both = Both" Belnap.Both (Belnap.belnapNot Belnap.Both)
    ]

scalarAndTests :: Test
scalarAndTests =
  let -- Full 4x4 truth table per Wikipedia B4; rows/columns: N, T, F, B
      expected =
        [ [Belnap.Unknown, Belnap.Unknown, Belnap.False, Belnap.False]
        , [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]
        , [Belnap.False, Belnap.False, Belnap.False, Belnap.False]
        , [Belnap.False, Belnap.Both, Belnap.False, Belnap.Both]
        ]
      cases =
        [ assertEqual
            (show a ++ " AND " ++ show b)
            (expected !! i !! j)
            (Belnap.belnapAnd a b)
        | (i, a) <- zip [0 ..] variants
        , (j, b) <- zip [0 ..] variants
        ]
   in group "scalar AND truth table" cases

scalarOrTests :: Test
scalarOrTests =
  let -- Full 4x4 truth table per Wikipedia B4; rows/columns: N, T, F, B
      expected =
        [ [Belnap.Unknown, Belnap.True, Belnap.Unknown, Belnap.True]
        , [Belnap.True, Belnap.True, Belnap.True, Belnap.True]
        , [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]
        , [Belnap.True, Belnap.True, Belnap.Both, Belnap.Both]
        ]
      cases =
        [ assertEqual
            (show a ++ " OR " ++ show b)
            (expected !! i !! j)
            (Belnap.belnapOr a b)
        | (i, a) <- zip [0 ..] variants
        , (j, b) <- zip [0 ..] variants
        ]
   in group "scalar OR truth table" cases

scalarMergeTests :: Test
scalarMergeTests =
  let -- Full 4x4 truth table; rows/columns: N, T, F, B
      expected =
        [ [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]
        , [Belnap.True, Belnap.True, Belnap.Both, Belnap.Both]
        , [Belnap.False, Belnap.Both, Belnap.False, Belnap.Both]
        , [Belnap.Both, Belnap.Both, Belnap.Both, Belnap.Both]
        ]
      cases =
        [ assertEqual
            (show a ++ " merge " ++ show b)
            (expected !! i !! j)
            (Belnap.merge a b)
        | (i, a) <- zip [0 ..] variants
        , (j, b) <- zip [0 ..] variants
        ]
   in group "scalar merge truth table" cases

scalarConsensusTests :: Test
scalarConsensusTests =
  let -- Full 4x4 truth table; rows/columns: N, T, F, B
      expected =
        [ [Belnap.Unknown, Belnap.Unknown, Belnap.Unknown, Belnap.Unknown]
        , [Belnap.Unknown, Belnap.True, Belnap.Unknown, Belnap.True]
        , [Belnap.Unknown, Belnap.Unknown, Belnap.False, Belnap.False]
        , [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]
        ]
      cases =
        [ assertEqual
            (show a ++ " consensus " ++ show b)
            (expected !! i !! j)
            (Belnap.consensus a b)
        | (i, a) <- zip [0 ..] variants
        , (j, b) <- zip [0 ..] variants
        ]
   in group "scalar consensus truth table" cases

scalarQueryTests :: Test
scalarQueryTests =
  group
    "scalar queries"
    [ assertBool "Unknown is not known" (not (Belnap.isKnown Belnap.Unknown))
    , assertBool "True is known" (Belnap.isKnown Belnap.True)
    , assertBool "False is known" (Belnap.isKnown Belnap.False)
    , assertBool "Both is known" (Belnap.isKnown Belnap.Both)
    , assertBool "Unknown is not determined" (not (Belnap.isDetermined Belnap.Unknown))
    , assertBool "True is determined" (Belnap.isDetermined Belnap.True)
    , assertBool "False is determined" (Belnap.isDetermined Belnap.False)
    , assertBool "Both is not determined" (not (Belnap.isDetermined Belnap.Both))
    , assertBool "Unknown is not contradicted" (not (Belnap.isContradicted Belnap.Unknown))
    , assertBool "True is not contradicted" (not (Belnap.isContradicted Belnap.True))
    , assertBool "False is not contradicted" (not (Belnap.isContradicted Belnap.False))
    , assertBool "Both is contradicted" (Belnap.isContradicted Belnap.Both)
    , assertEqual "Unknown toBool" Nothing (Belnap.toBool Belnap.Unknown)
    , assertEqual "True toBool" (Just True) (Belnap.toBool Belnap.True)
    , assertEqual "False toBool" (Just False) (Belnap.toBool Belnap.False)
    , assertEqual "Both toBool" Nothing (Belnap.toBool Belnap.Both)
    ]

-- BelnapVec tests

vecGetSetTests :: Test
vecGetSetTests =
  let v0 = Belnap.mkBelnapVec :: BelnapVec 4
      v1 = Belnap.set (fi 0) Belnap.Unknown v0
      v2 = Belnap.set (fi 1) Belnap.True v1
      v3 = Belnap.set (fi 2) Belnap.False v2
      v4 = Belnap.set (fi 3) Belnap.Both v3
   in group
        "vec get/set all four values"
        [ assertEqual "get 0 = Unknown" Belnap.Unknown (Belnap.get (fi 0) v4)
        , assertEqual "get 1 = True" Belnap.True (Belnap.get (fi 1) v4)
        , assertEqual "get 2 = False" Belnap.False (Belnap.get (fi 2) v4)
        , assertEqual "get 3 = Both" Belnap.Both (Belnap.get (fi 3) v4)
        ]

vecBulkAndTests :: Test
vecBulkAndTests =
  let a = Belnap.allTrue :: BelnapVec 64
      b = Belnap.allFalse :: BelnapVec 64
      c = Belnap.vecAnd a b
   in group
        "vec bulk AND"
        [assertBool "allTrue AND allFalse = allFalse" (Belnap.isAllFalse c)]

vecBulkOrTests :: Test
vecBulkOrTests =
  let a = Belnap.allFalse :: BelnapVec 64
      b = Belnap.allTrue :: BelnapVec 64
      c = Belnap.vecOr a b
   in group
        "vec bulk OR"
        [assertBool "allFalse OR allTrue = allTrue" (Belnap.isAllTrue c)]

vecBulkNotTests :: Test
vecBulkNotTests =
  let a = Belnap.allTrue :: BelnapVec 100
      b = Belnap.vecNot a
      c = Belnap.vecNot b
   in group
        "vec bulk NOT"
        [ assertBool "not allTrue = allFalse" (Belnap.isAllFalse b)
        , assertBool "not (not allTrue) = allTrue" (Belnap.isAllTrue c)
        ]

vecBulkMergeTests :: Test
vecBulkMergeTests =
  let a = Belnap.allTrue :: BelnapVec 64
      b = Belnap.allFalse :: BelnapVec 64
      c = Belnap.vecMerge a b
   in group
        "vec bulk merge"
        [ assertEqual "count both" 64 (Belnap.countBoth c)
        , assertEqual "count true" 0 (Belnap.countTrue c)
        , assertEqual "count false" 0 (Belnap.countFalse c)
        , assertEqual "count unknown" 0 (Belnap.countUnknown c)
        ]

vecIsConsistentTests :: Test
vecIsConsistentTests =
  let consistent1 = Belnap.allTrue :: BelnapVec 64
      consistent2 =
        Belnap.set
          (fi 1)
          Belnap.False
          (Belnap.set (fi 0) Belnap.True (Belnap.mkBelnapVec :: BelnapVec 10))
      inconsistent = Belnap.set (fi 2) Belnap.Both consistent2
   in group
        "vec is consistent"
        [ assertBool "allTrue is consistent" (Belnap.isConsistent consistent1)
        , assertBool "True/False mix is consistent" (Belnap.isConsistent consistent2)
        , assertBool "vector with Both is not consistent" (not (Belnap.isConsistent inconsistent))
        ]

vecIsAllDeterminedTests :: Test
vecIsAllDeterminedTests =
  let v0 = Belnap.mkBelnapVec :: BelnapVec 4
      v1 = Belnap.set (fi 0) Belnap.True v0
      v2 = Belnap.set (fi 1) Belnap.False v1
      v3 = Belnap.set (fi 2) Belnap.True v2
      vDet = Belnap.set (fi 3) Belnap.False v3
      vUnk = Belnap.set (fi 3) Belnap.Unknown v3
      vBoth = Belnap.set (fi 3) Belnap.Both v3
   in group
        "vec is all determined"
        [ assertBool "True/False mix is all determined" (Belnap.isAllDetermined vDet)
        , assertBool "Unknown makes it not all determined" (not (Belnap.isAllDetermined vUnk))
        , assertBool "Both makes it not all determined" (not (Belnap.isAllDetermined vBoth))
        ]

vecCountsTests :: Test
vecCountsTests =
  let v0 = Belnap.mkBelnapVec :: BelnapVec 10
      v1 = Belnap.set (fi 0) Belnap.True v0
      v2 = Belnap.set (fi 1) Belnap.True v1
      v3 = Belnap.set (fi 2) Belnap.False v2
      v = Belnap.set (fi 3) Belnap.Both v3
   in group
        "vec counts"
        [ assertEqual "count true" 2 (Belnap.countTrue v)
        , assertEqual "count false" 1 (Belnap.countFalse v)
        , assertEqual "count both" 1 (Belnap.countBoth v)
        , assertEqual "count unknown" 6 (Belnap.countUnknown v)
        ]

vecTruncateTests :: Test
vecTruncateTests =
  group
    "vec resize"
    [ let v = Belnap.resize (Belnap.allTrue :: BelnapVec 100) :: BelnapVec 100
       in group
            "same width is no-op"
            [ assertEqual "width" 100 v.width
            , assertBool "still all true" (Belnap.isAllTrue v)
            ]
    , let v = Belnap.resize (Belnap.allTrue :: BelnapVec 200) :: BelnapVec 65
       in group
            "shrink across word boundary"
            [ assertEqual "width" 65 v.width
            , assertBool "all true after shrink" (Belnap.isAllTrue v)
            , assertEqual "count true" 65 (Belnap.countTrue v)
            ]
    , let v = Belnap.resize (Belnap.allFalse :: BelnapVec 60) :: BelnapVec 200
       in group
            "grow across word boundary"
            [ assertEqual "width" 200 v.width
            , assertEqual "count false" 60 (Belnap.countFalse v)
            , assertEqual "count unknown" 140 (Belnap.countUnknown v)
            ]
    ]

-- Instance law tests

latticeLawTests :: (Eq a, Show a, Lattice a) => String -> [a] -> Test
latticeLawTests label vs =
  group
    ("Lattice laws: " ++ label)
    [ group
        "join associativity"
        [ assertEqual
            (show x ++ " \\/ (" ++ show y ++ " \\/ " ++ show z ++ ")")
            (x \/ (y \/ z))
            ((x \/ y) \/ z)
        | x <- vs
        , y <- vs
        , z <- vs
        ]
    , group
        "join commutativity"
        [ assertEqual (show x ++ " \\/ " ++ show y) (x \/ y) (y \/ x)
        | x <- vs
        , y <- vs
        ]
    , group
        "join idempotency"
        [ assertEqual (show x ++ " \\/ " ++ show x) x (x \/ x)
        | x <- vs
        ]
    , group
        "meet associativity"
        [ assertEqual
            (show x ++ " /\\ (" ++ show y ++ " /\\ " ++ show z ++ ")")
            (x /\ (y /\ z))
            ((x /\ y) /\ z)
        | x <- vs
        , y <- vs
        , z <- vs
        ]
    , group
        "meet commutativity"
        [ assertEqual (show x ++ " /\\ " ++ show y) (x /\ y) (y /\ x)
        | x <- vs
        , y <- vs
        ]
    , group
        "meet idempotency"
        [ assertEqual (show x ++ " /\\ " ++ show x) x (x /\ x)
        | x <- vs
        ]
    , group
        "absorption: x \\/ (x /\\ y)"
        [ assertEqual (show x ++ " \\/ (" ++ show x ++ " /\\ " ++ show y ++ ")") x (x \/ (x /\ y))
        | x <- vs
        , y <- vs
        ]
    , group
        "absorption: x /\\ (x \\/ y)"
        [ assertEqual (show x ++ " /\\ (" ++ show x ++ " \\/ " ++ show y ++ ")") x (x /\ (x \/ y))
        | x <- vs
        , y <- vs
        ]
    ]

boundedLawTests :: (Eq a, Show a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) => String -> [a] -> Test
boundedLawTests label vs =
  group
    ("Bounded lattice laws: " ++ label)
    [ group
        "bottom identity: x \\/ bottom"
        [ assertEqual (show x ++ " \\/ bottom") x (x \/ bottom)
        | x <- vs
        ]
    , group
        "top identity: x /\\ top"
        [ assertEqual (show x ++ " /\\ top") x (x /\ top)
        | x <- vs
        ]
    ]

asTruthVariants :: [AsTruth Belnap]
asTruthVariants = map AsTruth variants

asKnowledgeVariants :: [AsKnowledge Belnap]
asKnowledgeVariants = map AsKnowledge variants

-- QuickCheck properties for BelnapVec instance laws

genBelnapVec :: forall n. (KnownNat n) => Gen (BelnapVec n)
genBelnapVec = do
  let w = fromIntegral (natVal (Proxy @n))
  values <- vectorOf w (elements variants)
  pure $
    foldl
      (\v (i, b) -> maybe v (\idx -> Belnap.set idx b v) (Belnap.packFinite (fromIntegral i)))
      Belnap.mkBelnapVec
      (zip [0 :: Int ..] values)

instance (KnownNat n) => Arbitrary (BelnapVec n) where
  arbitrary = genBelnapVec
  shrink _ = []

vecLatticeQCProps :: (Eq a, Show a, Lattice a) => String -> (BelnapVec 200 -> a) -> [(String, Property)]
vecLatticeQCProps label wrap =
  let gen = wrap <$> (arbitrary :: Gen (BelnapVec 200))
      gen2 = (,) <$> gen <*> gen
      gen3 = (,,) <$> gen <*> gen <*> gen
   in [ (label ++ " join associativity", forAll gen3 $ \(x, y, z) -> x \/ (y \/ z) === (x \/ y) \/ z)
      , (label ++ " join commutativity", forAll gen2 $ \(x, y) -> x \/ y === y \/ x)
      , (label ++ " join idempotency", forAll gen $ \x -> x \/ x === x)
      , (label ++ " meet associativity", forAll gen3 $ \(x, y, z) -> x /\ (y /\ z) === (x /\ y) /\ z)
      , (label ++ " meet commutativity", forAll gen2 $ \(x, y) -> x /\ y === y /\ x)
      , (label ++ " meet idempotency", forAll gen $ \x -> x /\ x === x)
      , (label ++ " absorption (join)", forAll gen2 $ \(x, y) -> x \/ (x /\ y) === x)
      , (label ++ " absorption (meet)", forAll gen2 $ \(x, y) -> x /\ (x \/ y) === x)
      ]

vecBoundedQCProps ::
  (Eq a, Show a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) =>
  String ->
  (BelnapVec 200 -> a) ->
  [(String, Property)]
vecBoundedQCProps label wrap =
  let gen = wrap <$> (arbitrary :: Gen (BelnapVec 200))
   in [ (label ++ " bottom identity", forAll gen $ \x -> x \/ bottom === x)
      , (label ++ " top identity", forAll gen $ \x -> x /\ top === x)
      ]

quickCheckProps :: [(String, Property)]
quickCheckProps =
  vecLatticeQCProps "BelnapVec AsTruth" AsTruth
    ++ vecLatticeQCProps "BelnapVec AsKnowledge" AsKnowledge
    ++ vecBoundedQCProps "BelnapVec AsTruth" AsTruth
    ++ vecBoundedQCProps "BelnapVec AsKnowledge" AsKnowledge

runQuickCheckProps :: [(String, Property)] -> IO Bool
runQuickCheckProps props = do
  results <- mapM (\(name, prop) -> putStrLn ("Testing " ++ name ++ "...") >> quickCheckResult prop) props
  pure (all isSuccess results)

-- Top-level test runner

allTests :: Test
allTests =
  group
    "Hbt.Attic.Belnap"
    [ scalarNotTests
    , scalarAndTests
    , scalarOrTests
    , scalarMergeTests
    , scalarConsensusTests
    , scalarQueryTests
    , vecGetSetTests
    , vecBulkAndTests
    , vecBulkOrTests
    , vecBulkNotTests
    , vecBulkMergeTests
    , vecIsConsistentTests
    , vecIsAllDeterminedTests
    , vecCountsTests
    , vecTruncateTests
    , latticeLawTests "AsTruth" asTruthVariants
    , latticeLawTests "AsKnowledge" asKnowledgeVariants
    , boundedLawTests "AsTruth" asTruthVariants
    , boundedLawTests "AsKnowledge" asKnowledgeVariants
    ]

main :: IO ()
main = do
  let result = runTest allTests
  putStrLn (resultToString result)
  qcOk <- runQuickCheckProps quickCheckProps
  unless (resultIsPassed result && qcOk) exitFailure
