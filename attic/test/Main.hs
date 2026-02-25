module Main (main) where

import Control.Monad (unless)
import Hbt.Attic.Belnap (OutOfBounds (..))
import Hbt.Attic.Belnap qualified as Belnap
import System.Exit (exitFailure)
import Test.Dwergaz

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
  let variants = [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]
      -- Full 4x4 truth table per Wikipedia B4; rows/columns: N, T, F, B
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
  let variants = [Belnap.Unknown, Belnap.True, Belnap.False, Belnap.Both]
      -- Full 4x4 truth table per Wikipedia B4; rows/columns: N, T, F, B
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
  group
    "scalar merge"
    [ assertEqual "Unknown merge Unknown" Belnap.Unknown (Belnap.merge Belnap.Unknown Belnap.Unknown)
    , assertEqual "Unknown merge True" Belnap.True (Belnap.merge Belnap.Unknown Belnap.True)
    , assertEqual "Unknown merge False" Belnap.False (Belnap.merge Belnap.Unknown Belnap.False)
    , assertEqual "True merge False" Belnap.Both (Belnap.merge Belnap.True Belnap.False)
    , assertEqual "Both merge True" Belnap.Both (Belnap.merge Belnap.Both Belnap.True)
    , assertEqual "Both merge False" Belnap.Both (Belnap.merge Belnap.Both Belnap.False)
    , assertEqual "Both merge Unknown" Belnap.Both (Belnap.merge Belnap.Both Belnap.Unknown)
    , assertEqual "True merge True" Belnap.True (Belnap.merge Belnap.True Belnap.True)
    , assertEqual "False merge False" Belnap.False (Belnap.merge Belnap.False Belnap.False)
    ]

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
  let v0 = Belnap.mkBelnapVec 4
      v1 = Belnap.set 0 Belnap.Unknown v0
      v2 = Belnap.set 1 Belnap.True v1
      v3 = Belnap.set 2 Belnap.False v2
      v4 = Belnap.set 3 Belnap.Both v3
   in group
        "vec get/set all four values"
        [ assertEqual "get 0 = Unknown" (Right Belnap.Unknown) (Belnap.get 0 v4)
        , assertEqual "get 1 = True" (Right Belnap.True) (Belnap.get 1 v4)
        , assertEqual "get 2 = False" (Right Belnap.False) (Belnap.get 2 v4)
        , assertEqual "get 3 = Both" (Right Belnap.Both) (Belnap.get 3 v4)
        ]

vecBulkAndTests :: Test
vecBulkAndTests =
  let a = Belnap.allTrue 64
      b = Belnap.allFalse 64
      c = Belnap.vecAnd a b
   in group
        "vec bulk AND"
        [assertBool "allTrue AND allFalse = allFalse" (Belnap.isAllFalse c)]

vecBulkOrTests :: Test
vecBulkOrTests =
  let a = Belnap.allFalse 64
      b = Belnap.allTrue 64
      c = Belnap.vecOr a b
   in group
        "vec bulk OR"
        [assertBool "allFalse OR allTrue = allTrue" (Belnap.isAllTrue c)]

vecBulkNotTests :: Test
vecBulkNotTests =
  let a = Belnap.allTrue 100
      b = Belnap.vecNot a
      c = Belnap.vecNot b
   in group
        "vec bulk NOT"
        [ assertBool "not allTrue = allFalse" (Belnap.isAllFalse b)
        , assertBool "not (not allTrue) = allTrue" (Belnap.isAllTrue c)
        ]

vecBulkMergeTests :: Test
vecBulkMergeTests =
  let a = Belnap.allTrue 64
      b = Belnap.allFalse 64
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
  let consistent1 = Belnap.allTrue 64
      consistent2 = Belnap.set 1 Belnap.False (Belnap.set 0 Belnap.True (Belnap.mkBelnapVec 10))
      inconsistent = Belnap.set 2 Belnap.Both consistent2
   in group
        "vec is consistent"
        [ assertBool "allTrue is consistent" (Belnap.isConsistent consistent1)
        , assertBool "True/False mix is consistent" (Belnap.isConsistent consistent2)
        , assertBool "vector with Both is not consistent" (not (Belnap.isConsistent inconsistent))
        ]

vecIsAllDeterminedTests :: Test
vecIsAllDeterminedTests =
  let v0 = Belnap.mkBelnapVec 4
      v1 = Belnap.set 0 Belnap.True v0
      v2 = Belnap.set 1 Belnap.False v1
      v3 = Belnap.set 2 Belnap.True v2
      vDet = Belnap.set 3 Belnap.False v3
      vUnk = Belnap.set 3 Belnap.Unknown v3
      vBoth = Belnap.set 3 Belnap.Both v3
   in group
        "vec is all determined"
        [ assertBool "True/False mix is all determined" (Belnap.isAllDetermined vDet)
        , assertBool "Unknown makes it not all determined" (not (Belnap.isAllDetermined vUnk))
        , assertBool "Both makes it not all determined" (not (Belnap.isAllDetermined vBoth))
        ]

vecCountsTests :: Test
vecCountsTests =
  let v0 = Belnap.mkBelnapVec 10
      v1 = Belnap.set 0 Belnap.True v0
      v2 = Belnap.set 1 Belnap.True v1
      v3 = Belnap.set 2 Belnap.False v2
      v = Belnap.set 3 Belnap.Both v3
   in group
        "vec counts"
        [ assertEqual "count true" 2 (Belnap.countTrue v)
        , assertEqual "count false" 1 (Belnap.countFalse v)
        , assertEqual "count both" 1 (Belnap.countBoth v)
        , assertEqual "count unknown" 6 (Belnap.countUnknown v)
        ]

vecAutoGrowTests :: Test
vecAutoGrowTests =
  let v0 = Belnap.mkBelnapVec 10
      v = Belnap.set 100 Belnap.Both v0
   in group
        "vec auto grow"
        [ assertEqual "width after out-of-bounds set" 101 v.width
        , assertEqual "get 100 = Both" (Right Belnap.Both) (Belnap.get 100 v)
        , assertEqual "get 50 = Unknown" (Right Belnap.Unknown) (Belnap.get 50 v)
        , assertEqual "get 200 = OutOfBounds" (Left OutOfBounds) (Belnap.get 200 v)
        ]

vecResizeTests :: Test
vecResizeTests =
  group
    "vec resize"
    [ let v = Belnap.resize 100 Belnap.Unknown (Belnap.allTrue 10)
       in group
            "grow with Unknown fill"
            [ assertEqual "width" 100 v.width
            , assertEqual "count true" 10 (Belnap.countTrue v)
            , assertEqual "count unknown" 90 (Belnap.countUnknown v)
            ]
    , let v = Belnap.resize 100 Belnap.Both (Belnap.allTrue 10)
       in group
            "grow with Both fill"
            [ assertEqual "width" 100 v.width
            , assertEqual "count true" 10 (Belnap.countTrue v)
            , assertEqual "count both" 90 (Belnap.countBoth v)
            ]
    , let v = Belnap.resize 100 Belnap.False (Belnap.allTrue 10)
       in group
            "grow with False fill"
            [ assertEqual "width" 100 v.width
            , assertEqual "count true" 10 (Belnap.countTrue v)
            , assertEqual "count false" 90 (Belnap.countFalse v)
            ]
    , let v = Belnap.resize 100 Belnap.True (Belnap.mkBelnapVec 10)
       in group
            "grow with True fill"
            [ assertEqual "width" 100 v.width
            , assertEqual "count unknown" 10 (Belnap.countUnknown v)
            , assertEqual "count true" 90 (Belnap.countTrue v)
            ]
    , let v = Belnap.resize 200 Belnap.True (Belnap.allFalse 60)
       in group
            "grow across word boundary"
            [ assertEqual "width" 200 v.width
            , assertEqual "count false" 60 (Belnap.countFalse v)
            , assertEqual "count true" 140 (Belnap.countTrue v)
            ]
    , let v = Belnap.resize 10 Belnap.False (Belnap.allTrue 100)
       in group
            "shrink"
            [ assertEqual "width" 10 v.width
            , assertBool "still all true after shrink" (Belnap.isAllTrue v)
            ]
    , let v = Belnap.resize 64 Belnap.True (Belnap.mkBelnapVec 0)
       in group
            "grow from empty with True"
            [ assertEqual "width" 64 v.width
            , assertBool "is all true" (Belnap.isAllTrue v)
            ]
    , let v = Belnap.resize 100 Belnap.False (Belnap.mkBelnapVec 0)
       in group
            "grow from empty with False"
            [ assertEqual "width" 100 v.width
            , assertBool "is all false" (Belnap.isAllFalse v)
            ]
    ]

vecTruncateTests :: Test
vecTruncateTests =
  group
    "vec truncate"
    [ let v = Belnap.truncate 100 (Belnap.allTrue 100)
       in group
            "truncate to same width is no-op"
            [ assertEqual "width" 100 v.width
            , assertBool "still all true" (Belnap.isAllTrue v)
            ]
    , let v = Belnap.truncate 65 (Belnap.allTrue 200)
       in group
            "truncate across word boundary"
            [ assertEqual "width" 65 v.width
            , assertBool "all true after truncate" (Belnap.isAllTrue v)
            , assertEqual "count true" 65 (Belnap.countTrue v)
            ]
    ]

vecAndDifferentWidthsTests :: Test
vecAndDifferentWidthsTests =
  let short =
        Belnap.set
          2
          Belnap.Both
          ( Belnap.set
              1
              Belnap.False
              (Belnap.set 0 Belnap.True (Belnap.mkBelnapVec 10))
          )
      long =
        Belnap.set
          99
          Belnap.True
          ( Belnap.set
              2
              Belnap.True
              ( Belnap.set
                  1
                  Belnap.True
                  (Belnap.set 0 Belnap.True (Belnap.mkBelnapVec 100))
              )
          )
      ab = Belnap.vecAnd short long
      ba = Belnap.vecAnd long short
   in group
        "vec AND different widths"
        [ assertEqual "ab width" 100 ab.width
        , assertEqual "ba width" 100 ba.width
        , assertEqual "ab == ba" ab ba
        , assertEqual "get 0: True AND True = True" (Right Belnap.True) (Belnap.get 0 ab)
        , assertEqual "get 1: False AND True = False" (Right Belnap.False) (Belnap.get 1 ab)
        , assertEqual "get 2: Both AND True = Both" (Right Belnap.Both) (Belnap.get 2 ab)
        , assertEqual "get 99: Unknown AND True = Unknown" (Right Belnap.Unknown) (Belnap.get 99 ab)
        , assertEqual "get 50: Unknown AND Unknown = Unknown" (Right Belnap.Unknown) (Belnap.get 50 ab)
        ]

vecOrDifferentWidthsTests :: Test
vecOrDifferentWidthsTests =
  let short =
        Belnap.set
          2
          Belnap.Both
          ( Belnap.set
              1
              Belnap.False
              (Belnap.set 0 Belnap.True (Belnap.mkBelnapVec 10))
          )
      long =
        Belnap.set
          99
          Belnap.False
          ( Belnap.set
              2
              Belnap.False
              ( Belnap.set
                  1
                  Belnap.True
                  (Belnap.set 0 Belnap.False (Belnap.mkBelnapVec 100))
              )
          )
      ab = Belnap.vecOr short long
      ba = Belnap.vecOr long short
   in group
        "vec OR different widths"
        [ assertEqual "ab width" 100 ab.width
        , assertEqual "ba width" 100 ba.width
        , assertEqual "ab == ba" ab ba
        , assertEqual "get 0: True OR False = True" (Right Belnap.True) (Belnap.get 0 ab)
        , assertEqual "get 1: False OR True = True" (Right Belnap.True) (Belnap.get 1 ab)
        , assertEqual "get 2: Both OR False = Both" (Right Belnap.Both) (Belnap.get 2 ab)
        , assertEqual "get 99: Unknown OR False = Unknown" (Right Belnap.Unknown) (Belnap.get 99 ab)
        , assertEqual "get 50: Unknown OR Unknown = Unknown" (Right Belnap.Unknown) (Belnap.get 50 ab)
        ]

vecMergeDifferentWidthsTests :: Test
vecMergeDifferentWidthsTests =
  let short =
        Belnap.set
          1
          Belnap.False
          (Belnap.set 0 Belnap.True (Belnap.mkBelnapVec 10))
      long =
        Belnap.set
          99
          Belnap.True
          ( Belnap.set
              1
              Belnap.True
              (Belnap.set 0 Belnap.False (Belnap.mkBelnapVec 100))
          )
      ab = Belnap.vecMerge short long
      ba = Belnap.vecMerge long short
   in group
        "vec merge different widths"
        [ assertEqual "ab width" 100 ab.width
        , assertEqual "ba width" 100 ba.width
        , assertEqual "ab == ba" ab ba
        , assertEqual "get 0: True merge False = Both" (Right Belnap.Both) (Belnap.get 0 ab)
        , assertEqual "get 1: False merge True = Both" (Right Belnap.Both) (Belnap.get 1 ab)
        , assertEqual "get 99: Unknown merge True = True" (Right Belnap.True) (Belnap.get 99 ab)
        , assertEqual "get 50: Unknown merge Unknown = Unknown" (Right Belnap.Unknown) (Belnap.get 50 ab)
        ]

vecImpliesDifferentWidthsTests :: Test
vecImpliesDifferentWidthsTests =
  let short = Belnap.allTrue 10
      long = Belnap.allTrue 100
      result = Belnap.vecImplies short long
   in group
        "vec implies different widths"
        [ assertEqual "width" 100 result.width
        , assertEqual "get 0: True implies True = True" (Right Belnap.True) (Belnap.get 0 result)
        , assertEqual "get 50: Unknown implies True = True" (Right Belnap.True) (Belnap.get 50 result)
        ]

-- Top-level test runner

allTests :: Test
allTests =
  group
    "Hbt.Attic.Belnap"
    [ scalarNotTests
    , scalarAndTests
    , scalarOrTests
    , scalarMergeTests
    , scalarQueryTests
    , vecGetSetTests
    , vecBulkAndTests
    , vecBulkOrTests
    , vecBulkNotTests
    , vecBulkMergeTests
    , vecIsConsistentTests
    , vecIsAllDeterminedTests
    , vecCountsTests
    , vecAutoGrowTests
    , vecResizeTests
    , vecTruncateTests
    , vecAndDifferentWidthsTests
    , vecOrDifferentWidthsTests
    , vecMergeDifferentWidthsTests
    , vecImpliesDifferentWidthsTests
    ]

main :: IO ()
main = do
  let result = runTest allTests
  putStrLn (resultToString result)
  unless (resultIsPassed result) exitFailure
