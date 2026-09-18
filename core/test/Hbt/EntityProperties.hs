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
newtype SmallEntity = MkSmallEntity Entity
  deriving stock (Show)

instants :: [Int64]
instants = [1000, 2000, 3000]

exampleURI :: URI
exampleURI = either (error . show) id (URI.parse "https://example.com")

instance Arbitrary SmallEntity where
  arbitrary = do
    -- An absent creation time is reachable: 'Entity.empty' starts the HTML
    -- parser's fold, and an anchor with no ADD_DATE parses to one.
    created <- frequency [(1, pure Nothing), (4, Just <$> elements instants)]
    updates <- sublistOf instants
    let base = mempty {uri = exampleURI, updatedAt = Set.fromList (map Time.fromSeconds updates)}
    pure . MkSmallEntity $ case created of
      Nothing -> base
      Just secs -> base {createdAt = Entity.mkCreatedAt (Time.fromSeconds secs)}

  shrink (MkSmallEntity e) =
    [MkSmallEntity e {updatedAt = Set.delete t e.updatedAt} | t <- Set.toList e.updatedAt]

props :: [(String, Property)]
props =
  [ ("Entity <> is associative", property associative)
  , ("merged timestamps do not depend on merge order", property timestampsCommute)
  ]
  where
    associative (MkSmallEntity a) (MkSmallEntity b) (MkSmallEntity c) =
      ((a <> b) <> c) === (a <> (b <> c))
    timestampsCommute (MkSmallEntity a) (MkSmallEntity b) =
      timestamps (a <> b) === timestamps (b <> a)
    timestamps e = (e.createdAt, e.updatedAt)
