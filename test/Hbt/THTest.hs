module Hbt.THTest where

import Hbt.TH
import Language.Haskell.TH
import Test.Dwergaz
import TestUtilities (testResults)

extractorTests :: Test
extractorTests =
  group
    "Constructor extraction"
    [ assertEqual "extractEnumConstructorNames extracts from normal constructors" 2 $
        let constructors = [NormalC (mkName "From") [], NormalC (mkName "To") []]
            names = extractEnumConstructorNames constructors
         in length names
    , assertEqual "extractEnumConstructorNames extracts from GADT constructors" 1 $
        let constructors = [GadtC [mkName "From"] [] (ConT (mkName "Flow"))]
            names = extractEnumConstructorNames constructors
         in length names
    ]

validationTests :: Test
validationTests =
  group
    "Type validation"
    [ assertBool "isValidForEnum matches exact types" $
        let resultType = AppT (ConT (mkName "Format")) (ConT (mkName "From"))
            targetType = ConT (mkName "From")
         in isValidForEnum resultType targetType
    , assertBool "isValidForEnum matches polymorphic types" $
        let resultType = AppT (ConT (mkName "Format")) (VarT (mkName "f"))
            targetType = ConT (mkName "From")
         in isValidForEnum resultType targetType
    , assertBool "isValidForEnum rejects mismatched types" $
        let resultType = AppT (ConT (mkName "Format")) (ConT (mkName "To"))
            targetType = ConT (mkName "From")
         in not $ isValidForEnum resultType targetType
    ]

allTests :: Test
allTests =
  group
    "Hbt.TH"
    [ extractorTests
    , validationTests
    ]

-- Run tests
results :: (String, Bool)
results = testResults "Hbt.TH" allTests
