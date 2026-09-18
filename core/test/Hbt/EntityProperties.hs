{-# LANGUAGE OverloadedStrings #-}

-- | Laws that decide the merge rule, as properties rather than examples.
--
-- henrytill/hbt-data#36 settled the rule for all four implementations because
-- it is the one that keeps '<>' associative; the fixed triples in
-- "Hbt.CollectionTest" pin the counterexample that argument turns on, and this
-- pins the law they are an instance of.
--
-- Only the timestamps are order-independent, not the whole entity: 'uri' is a
-- 'First' and the flag fields are 'Last', so a merge decides them by order on
-- purpose. A commutativity property over entities would be false, and would
-- pass only for a generator that never varied those fields.
module Hbt.EntityProperties (props) where

import Data.Int (Int64)
import Data.Set qualified as Set
import Hbt.Entity (Entity (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Test.QuickCheck

-- | An entity over a deliberately small domain: one URI and a handful of
-- instants, so that creation times collide and histories overlap by chance.
-- The shape the law turns on -- a history holding an instant equal to its own
-- creation time -- then arises on its own rather than by construction.
--
-- Deliberately *not* normalized. 'Semigroup' is kept while 'Monoid' is dropped
-- precisely because associativity holds for every value of the type and the
-- identity law does not, so the property that carries that argument has to
-- range over values the normal form excludes. Use 'NormalEntity' for a claim
-- that is only made about entities the program can reach.
newtype AnyEntity = MkAnyEntity Entity
  deriving stock (Show)

-- | An 'AnyEntity' put in normal form: what every construction in "Hbt.Entity"
-- produces, and the only entities 'Entity.empty' is a unit for.
newtype NormalEntity = MkNormalEntity Entity
  deriving stock (Show)

instants :: [Int64]
instants = [1000, 2000, 3000]

exampleURI :: URI
exampleURI = either (error . show) id (URI.parse "https://example.com")

instance Arbitrary AnyEntity where
  arbitrary = do
    -- An absent creation time is reachable: 'Entity.empty' starts the HTML
    -- parser's fold, and an anchor with no ADD_DATE parses to one.
    created <- frequency [(1, pure Nothing), (4, Just <$> elements instants)]
    updates <- sublistOf instants
    let base = Entity.empty {uri = exampleURI, updatedAt = Set.fromList (map Time.fromSeconds updates)}
    pure . MkAnyEntity $ case created of
      Nothing -> base
      Just secs -> base {createdAt = Entity.mkCreatedAt (Time.fromSeconds secs)}

  shrink (MkAnyEntity e) =
    [MkAnyEntity e {updatedAt = Set.delete t e.updatedAt} | t <- Set.toList e.updatedAt]

-- Deleting an update preserves the normal form, so the shrinker carries over.
instance Arbitrary NormalEntity where
  arbitrary = MkNormalEntity . Entity.normalize . getAny <$> arbitrary
  shrink (MkNormalEntity e) = MkNormalEntity . getAny <$> shrink (MkAnyEntity e)

getAny :: AnyEntity -> Entity
getAny (MkAnyEntity e) = e

props :: [(String, Property)]
props =
  [ ("Entity <> is associative", property associative)
  , ("merged timestamps do not depend on merge order", property timestampsCommute)
  , ("Entity.empty is a left unit", property leftUnit)
  , ("Entity.empty is a right unit", property rightUnit)
  , ("<> is idempotent on a normalized entity", property idempotent)
  , ("<> preserves normal form", property preservesNormalForm)
  ]
  where
    -- These three range over AnyEntity: they are claims about the type, and
    -- associativity in particular is the whole reason 'Semigroup' survived the
    -- removal of 'Monoid'. Normalizing the generator would quietly narrow them
    -- to the reachable values and stop testing what they are cited for.
    associative (MkAnyEntity a) (MkAnyEntity b) (MkAnyEntity c) =
      ((a <> b) <> c) === (a <> (b <> c))
    timestampsCommute (MkAnyEntity a) (MkAnyEntity b) =
      timestamps (a <> b) === timestamps (b <> a)
    timestamps e = (e.createdAt, e.updatedAt)
    preservesNormalForm (MkAnyEntity a) (MkAnyEntity b) =
      Entity.normalize (a <> b) === (a <> b)
    -- These three are false for the type and true for every entity the program
    -- can reach, which is what normalizing at construction and decoding buys,
    -- and why there is no 'Monoid' instance to assert them. An entity whose
    -- history repeats its own creation time is the counterexample to all
    -- three. See henrytill/hbt-data#38.
    leftUnit (MkNormalEntity a) = (Entity.empty <> a) === a
    rightUnit (MkNormalEntity a) = (a <> Entity.empty) === a
    idempotent (MkNormalEntity a) = (a <> a) === a
