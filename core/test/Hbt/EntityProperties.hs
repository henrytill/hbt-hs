{-# LANGUAGE OverloadedStrings #-}

-- | Laws that decide the merge rule, as properties rather than examples.
--
-- henrytill/hbt-data#36 settled the rule for all four implementations because
-- it is the one that keeps '<>' associative; the fixed triples in
-- "Hbt.CollectionTest" pin the counterexample that argument turns on, and this
-- pins the law they are an instance of.
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
    created <- elements instants
    updates <- sublistOf instants
    let entity = Entity.mkEntity exampleURI (Time.fromSeconds created) Nothing Set.empty
    pure . MkSmallEntity $ entity {updatedAt = Set.fromList (map Time.fromSeconds updates)}

props :: [(String, Property)]
props =
  [ ("Entity <> is associative", property associative)
  , ("Entity <> is commutative", property commutative)
  ]
  where
    associative (MkSmallEntity a) (MkSmallEntity b) (MkSmallEntity c) =
      ((a <> b) <> c) === (a <> (b <> c))
    commutative (MkSmallEntity a) (MkSmallEntity b) =
      (a <> b) === (b <> a)
