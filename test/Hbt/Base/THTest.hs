{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}

module Hbt.Base.THTest where

import Hbt.Base.TH
import Test.Dwergaz
import TestUtilities (testResults)

data RegularFlow = RegularFrom | RegularTo
  deriving (Eq, Show)

data RegularFormat (f :: RegularFlow) where
  RegularJSON :: RegularFormat RegularFrom
  RegularXML :: RegularFormat RegularFrom
  RegularHTML :: RegularFormat f
  RegularYAML :: RegularFormat RegularTo

deriving instance Eq (RegularFormat f)

deriving instance Show (RegularFormat f)

$(deriveAllConstructors ''RegularFormat ''RegularFlow)

type data TypeDataFlow = TypeDataFrom | TypeDataTo

data TypeDataFormat (f :: TypeDataFlow) where
  TypeDataJSON :: TypeDataFormat TypeDataFrom
  TypeDataXML :: TypeDataFormat TypeDataFrom
  TypeDataHTML :: TypeDataFormat f
  TypeDataYAML :: TypeDataFormat TypeDataTo

deriving instance Eq (TypeDataFormat f)

deriving instance Show (TypeDataFormat f)

$(deriveAllConstructors ''TypeDataFormat ''TypeDataFlow)

constructorGenerationTests :: Test
constructorGenerationTests =
  group
    "Constructor generation"
    [ assertEqual
        "Regular data: exact RegularFrom constructors"
        [RegularJSON, RegularXML, RegularHTML]
        allRegularFromConstructors
    , assertEqual
        "Regular data: exact RegularTo constructors"
        [RegularHTML, RegularYAML]
        allRegularToConstructors
    , assertEqual
        "Type data: exact TypeDataFrom constructors"
        [TypeDataJSON, TypeDataXML, TypeDataHTML]
        allTypeDataFromConstructors
    , assertEqual
        "Type data: exact TypeDataTo constructors"
        [TypeDataHTML, TypeDataYAML]
        allTypeDataToConstructors
    ]

allTests :: Test
allTests =
  group
    "Hbt.Base.TH"
    [ constructorGenerationTests
    ]

results :: (String, Bool)
results = testResults "Hbt.Base.TH" allTests
