{-# LANGUAGE TemplateHaskell #-}

module Flow.TH where

import Flow (Flow (..))
import Language.Haskell.TH

-- | Test if constructor result type matches target flow.
--
-- Determines whether a GADT constructor's result type is compatible
-- with a specific flow direction.
isValidForFlow ::
  -- | The result type of a GADT constructor (e.g., @SomeType From@ for some constructor)
  Type ->
  -- | The target flow type we're checking against (e.g., 'From' or 'To')
  Type ->
  -- | True if the constructor can be used with the target flow
  Bool
isValidForFlow (AppT _ flowType) targetFlow | flowType == targetFlow = True
isValidForFlow (AppT _ (VarT _)) _ = True -- Polymorphic constructors match any flow
isValidForFlow _ _ = False

-- | Check if a GADT constructor works with a specific flow.
--
-- Examines a constructor's type signature to determine compatibility
-- with the target flow direction.
checkConstructorForFlow ::
  -- | The target flow type we want (e.g., 'From' or 'To')
  Type ->
  -- | A GADT constructor
  Con ->
  -- | Singleton list with constructor name if valid or empty list
  [Name]
checkConstructorForFlow targetFlowType (GadtC candidate@[_] _ resultType)
  | isValidForFlow resultType targetFlowType = candidate
  | otherwise = mempty
checkConstructorForFlow targetFlowType (ForallC _ _ (GadtC candidate@[_] _ resultType))
  | isValidForFlow resultType targetFlowType = candidate
  | otherwise = mempty
checkConstructorForFlow _ con =
  fail $ "Unsupported constructor: " ++ show con

-- | Filter constructors to find those valid for a given flow.
--
-- Returns only those constructors that are compatible with
-- the specified flow direction.
filterConstructorsForFlow ::
  -- | All constructors from the GADT
  [Con] ->
  -- | Quoted target flow type (@[t|From|]@ or @[t|To|]@)
  Q Type ->
  -- | List of constructor names that work with the target flow
  Q [Name]
filterConstructorsForFlow constructors flowTypeQ = do
  flowType <- flowTypeQ
  return $ foldMap (checkConstructorForFlow flowType) constructors

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
-- Introspects a GADT's constructors and generates separate lists
-- of constructors for each 'Flow' direction ('From' and 'To').
deriveAllConstructors ::
  -- | The name of the GADT type to analyze (e.g., @''SomeType@)
  Name ->
  -- | Generated function declarations for @allFromConstructors@ and @allToConstructors@
  Q [Dec]
deriveAllConstructors typeName = do
  TyConI (DataD [] _ _ _ constructors _) <- reify typeName
  fromConstructors <- filterConstructorsForFlow constructors [t|From|]
  toConstructors <- filterConstructorsForFlow constructors [t|To|]
  let typeConName = mkName (nameBase typeName)
  mappend
    [d|
      allFromConstructors :: [$(conT typeConName) From]
      allFromConstructors = $(makeConstructorList fromConstructors)
      |]
    [d|
      allToConstructors :: [$(conT typeConName) To]
      allToConstructors = $(makeConstructorList toConstructors)
      |]
