{-# LANGUAGE TemplateHaskell #-}

module Hbt.Base.TH where

import Control.Monad (liftM)
import Language.Haskell.TH

-- | Monadic version of concatMap
concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | Extract constructor names from an enumeration type
extractEnumConstructorNames :: [Con] -> [Name]
extractEnumConstructorNames constructors =
  [conName | NormalC conName [] <- constructors] ++ [conName | GadtC [conName] [] _ <- constructors]

-- | Test if constructor result type matches target enum value.
--
-- Determines whether a GADT constructor's result type is compatible
-- with a specific enumeration value.
isValidForEnum ::
  -- | The result type of a GADT constructor (e.g., @SomeType From@ for some constructor)
  Type ->
  -- | The target enum type we're checking against (e.g., 'From' or 'To')
  Type ->
  -- | True if the constructor can be used with the target enum value
  Bool
isValidForEnum (AppT _ (PromotedT conName)) (ConT targetName) | conName == targetName = True
isValidForEnum (AppT _ enumType) targetEnum | enumType == targetEnum = True
isValidForEnum (AppT _ (VarT _)) _ = True -- Polymorphic constructors match any enum value
isValidForEnum _ _ = False

-- | Check if a GADT constructor works with a specific enum value.
--
-- Examines a constructor's type signature to determine compatibility
-- with the target enumeration value.
checkConstructorForEnum ::
  -- | The target enum type we want (e.g., 'From' or 'To')
  Type ->
  -- | A GADT constructor
  Con ->
  -- | Singleton list with constructor name if valid or empty list
  [Name]
checkConstructorForEnum targetEnumType (GadtC candidate@[_] _ resultType)
  | isValidForEnum resultType targetEnumType = candidate
  | otherwise = mempty
checkConstructorForEnum targetEnumType (ForallC _ _ (GadtC candidate@[_] _ resultType))
  | isValidForEnum resultType targetEnumType = candidate
  | otherwise = mempty
checkConstructorForEnum _ con =
  fail $ "Unsupported constructor: " ++ show con

-- | Filter constructors to find those valid for a given enum value.
--
-- Returns only those constructors that are compatible with
-- the specified enumeration value.
filterConstructorsForEnum ::
  -- | All constructors from the GADT
  [Con] ->
  -- | Quoted target enum type (@[t|From|]@ or @[t|To|]@)
  Q Type ->
  -- | List of constructor names that work with the target enum value
  Q [Name]
filterConstructorsForEnum constructors enumTypeQ = do
  enumType <- enumTypeQ
  return $ foldMap (checkConstructorForEnum enumType) constructors

-- | Generate a list expression from constructor names.
--
-- Converts a list of constructor names into a quoted expression
-- that represents the list at compile time.
makeConstructorList ::
  -- | List of constructor names (e.g., @[JSON, XML, HTML]@)
  [Name] ->
  -- | Quoted expression representing the list
  Q Exp
makeConstructorList constructorNames = [e|$(listE (map conE constructorNames))|]

-- | Automatically generate constructor lists by analyzing GADT types.
--
-- Introspects both a GADT and its associated enumeration type to generate
-- separate constructor lists for each enumeration value.
deriveAllConstructors ::
  -- | The name of the GADT type to analyze (e.g., @''SomeType@)
  Name ->
  -- | The name of the enumeration type data (e.g., @''Flow@)
  Name ->
  -- | Generated function declarations for each enumeration constructor
  Q [Dec]
deriveAllConstructors gadtName enumName = do
  TyConI (DataD [] _ _ _ gadtConstructors _) <- reify gadtName
  enumInfo <- reify enumName
  let enumConstructors = case enumInfo of
        TyConI (DataD [] _ _ _ cons _) -> cons
        TyConI (TypeDataD _ _ _ cons) -> cons
        _ -> fail $ "Expected data or type data declaration for " ++ show enumName
  let typeConName = mkName (nameBase gadtName)
  let enumConstructorNames = extractEnumConstructorNames enumConstructors
  flip concatMapM enumConstructorNames $ \enumCon -> do
    matchingConstructors <- filterConstructorsForEnum gadtConstructors (conT enumCon)
    let functionName = mkName ("all" ++ nameBase enumCon ++ "Constructors")
    let functionType = [t|[$(conT typeConName) $(conT enumCon)]|]
    sequence
      [ sigD functionName functionType
      , funD functionName [clause [] (normalB (makeConstructorList matchingConstructors)) []]
      ]
