{-# LANGUAGE OverloadedStrings #-}

module Hbt.Entity
  ( Name (..)
  , Label (..)
  , Shared
  , mkShared
  , getShared
  , ToRead
  , mkToRead
  , getToRead
  , IsFeed
  , mkIsFeed
  , getIsFeed
  , Extended (..)
  , LastVisitedAt (..)
  , getLastVisitedAt
  , CreatedAt
  , mkCreatedAt
  , lookupCreatedAt
  , Entity (..)
  , mkEntity
  , empty
  , normalize
  , absorb
  , fromPost
  )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe
import Data.Monoid (Last (..))
import Data.Semigroup (Min (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hbt.Entity.Time (Time)
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI
import Hbt.Pinboard (Post (..))
import Hbt.Pinboard qualified as Pinboard

newtype Name = MkName {unName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype Label = MkLabel {unLabel :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype Shared = MkShared (Last Bool)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkShared :: Bool -> Shared
mkShared = MkShared . Last . Just

getShared :: Shared -> Maybe Bool
getShared (MkShared value) = getLast value

newtype ToRead = MkToRead (Last Bool)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkToRead :: Bool -> ToRead
mkToRead = MkToRead . Last . Just

getToRead :: ToRead -> Maybe Bool
getToRead (MkToRead value) = getLast value

newtype IsFeed = MkIsFeed (Last Bool)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

mkIsFeed :: Bool -> IsFeed
mkIsFeed = MkIsFeed . Last . Just

getIsFeed :: IsFeed -> Maybe Bool
getIsFeed (MkIsFeed value) = getLast value

newtype Extended = MkExtended {unExtended :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype LastVisitedAt = MkLastVisitedAt (Maybe Time)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

getLastVisitedAt :: LastVisitedAt -> Maybe Time
getLastVisitedAt (MkLastVisitedAt a) = a

instance Semigroup LastVisitedAt where
  MkLastVisitedAt a <> MkLastVisitedAt b = MkLastVisitedAt (max a b)

instance Monoid LastVisitedAt where
  mempty = MkLastVisitedAt Nothing

-- | The earliest creation time recorded for an entity.
--
-- The mirror image of 'LastVisitedAt': both wrap an optional 'Time' and keep
-- one end of the range on merge, the earliest here against the latest there,
-- with the absent value as the identity. Wrapping @Min@ in @Maybe@ gets both
-- instances from the ones they are built out of, rather than nominating a
-- sentinel time to stand in for "none recorded".
--
-- That absence reaches the wire: 'toJSON' omits @createdAt@ when there is
-- none, as it already did for the optional flags, and 'parseJSON' reads an
-- omitted or null key back as absent. It used to be written as the epoch,
-- which decoded as a real instant -- so an undated entity round-tripped into
-- one created on 1970-01-01 and merged differently afterwards. This
-- implementation was already merging the right way and only lacked the wire
-- form; henrytill/hbt-data#37 gave it one. A @createdAt@ of 0 is a real
-- instant and is kept.
newtype CreatedAt = MkCreatedAt (Maybe (Min Time))
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid)

mkCreatedAt :: Time -> CreatedAt
mkCreatedAt = MkCreatedAt . Just . Min

-- | The recorded creation time, if there is one.
lookupCreatedAt :: CreatedAt -> Maybe Time
lookupCreatedAt (MkCreatedAt a) = fmap getMin a

data Entity = MkEntity
  { uri :: URI
  , createdAt :: CreatedAt
  , updatedAt :: Set Time
  -- ^ Updates, never including 'createdAt'. HTML reads ADD_DATE and
  -- LAST_MODIFIED independently, so an anchor may state the same instant in
  -- both; 'normalize' is what makes that not survive, and html/bookmarks_simple
  -- pins it. The two are separate fields because a creation time is not an
  -- update, and an entity may have been created without ever being updated.
  --
  -- Nothing in the type enforces this - a record update can write any set.
  -- Every construction the program has normalizes; closing the type is
  -- henrytill/hbt-hs#54.
  , names :: Set Name
  , labels :: Set Label
  , isFeed :: IsFeed
  , shared :: Shared
  , toRead :: ToRead
  , extended :: Set Extended
  , lastVisitedAt :: LastVisitedAt
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON Entity where
  toJSON entity =
    object $
      [ "uri" .= entity.uri
      , "updatedAt" .= entity.updatedAt
      , "names" .= entity.names
      , "labels" .= entity.labels
      ]
        -- Omitted when there is none, rather than written as the epoch. Without a wire form for
        -- absence an undated entity decoded back as one created on 1970-01-01 and merged
        -- differently afterwards: henrytill/hbt-data#37.
        ++ ["createdAt" .= t | Just t <- [lookupCreatedAt entity.createdAt]]
        ++ ["isFeed" .= s | Just s <- [getIsFeed entity.isFeed]]
        ++ ["shared" .= s | Just s <- [getShared entity.shared]]
        ++ ["toRead" .= t | Just t <- [getToRead entity.toRead]]
        ++ ["extended" .= entity.extended | not (null entity.extended)]
        ++ ["lastVisitedAt" .= entity.lastVisitedAt | Maybe.isJust (getLastVisitedAt entity.lastVisitedAt)]

-- | Drop an update that merely repeats the creation time.
--
-- A timestamp equal to createdAt carries no information. An update strictly
-- *below* createdAt is a different thing and is untouched:
-- henrytill/hbt-data#34.
--
-- Held to it on the merge path by every implementation -- bookmarks_same_timestamp
-- is two anchors with equal ADD_DATE and no LAST_MODIFIED, and all four drop the
-- repeat. On the *parse* path this is so far only true here: given one anchor
-- stating the same instant in both attributes, Go, OCaml and Rust still record
-- the update. henrytill/hbt-data#38 is the decision that they should not, and
-- until it lands there they diverge from this on html/bookmarks_simple.
--
-- This is the whole of the normal form (henrytill/hbt-data#38), and three
-- places maintain it. '<>' ends here, so a merge that demotes the later
-- creation time to an update does not then record the earlier one twice.
-- 'FromJSON' and "Hbt.Parser.HTML" end here because both take a history from
-- input: HTML reads ADD_DATE and LAST_MODIFIED independently, so one anchor may
-- state the same instant in both - html/bookmarks_simple. The remaining
-- constructors - 'empty', 'mkEntity', 'fromPost' - are normal for a weaker
-- reason: they record no updates at all. One that learns to would have to
-- normalize too, and nothing but this note says so; henrytill/hbt-hs#54.
normalize :: Entity -> Entity
normalize entity =
  entity {updatedAt = maybe entity.updatedAt (`Set.delete` entity.updatedAt) (lookupCreatedAt entity.createdAt)}

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v -> do
    -- Absent and null both mean absent, as they do for the optional flags. A createdAt of 0 is a
    -- real instant and is kept: henrytill/hbt-data#37.
    createdAt <- v .:? "createdAt"
    fmap normalize $
      MkEntity
        <$> v .: "uri"
        <*> pure (MkCreatedAt (fmap Min createdAt))
        <*> v .: "updatedAt"
        <*> v .: "names"
        <*> v .: "labels"
        <*> v .:? "isFeed" .!= mempty
        <*> v .:? "shared" .!= mempty
        <*> v .:? "toRead" .!= mempty
        <*> v .:? "extended" .!= mempty
        <*> v .:? "lastVisitedAt" .!= mempty

-- | The updates of two merged entities: both histories and both creation
-- times.
--
-- Merging keeps the earlier creation time, so the later one would otherwise be
-- lost; it becomes an update instead. Putting *both* times in, and leaving it
-- to 'normalize' to take the winner back out, is what keeps '<>' associative:
-- each merge restores its operands' creation times, so no bracketing can lose
-- one. 'semigroupAssociativityTests' carries the counterexample that a rule
-- removing the winner only when the two times differ fails.
--
-- The price of that law is that a merge also removes an update equal to a
-- createdAt it did not move, which Go and Rust keep - henrytill/hbt-data#35,
-- where this is the argument from associativity for dropping it. An update
-- strictly below createdAt is untouched either way: henrytill/hbt-data#34.
mergedUpdates :: Entity -> Entity -> Set Time
mergedUpdates a b = a.updatedAt <> b.updatedAt <> creations
  where
    creations = Set.fromList (Maybe.mapMaybe lookupCreatedAt [a.createdAt, b.createdAt])

-- | Merging is field-wise, then 'normalize'd: the merged history holds both
-- creation times, and normalizing removes the one that won. The instant that
-- survives is typically another anchor's creation time -
-- bookmarks_superseded_creation.
--
-- There is deliberately no 'Monoid': see 'empty'.
instance Semigroup Entity where
  a <> b =
    normalize
      MkEntity
        { uri = a.uri <> b.uri
        , createdAt = a.createdAt <> b.createdAt
        , updatedAt = mergedUpdates a b
        , names = a.names <> b.names
        , labels = a.labels <> b.labels
        , isFeed = a.isFeed <> b.isFeed
        , shared = a.shared <> b.shared
        , toRead = a.toRead <> b.toRead
        , extended = a.extended <> b.extended
        , lastVisitedAt = a.lastVisitedAt <> b.lastVisitedAt
        }

-- | The entity every construction starts from.
--
-- This is deliberately not a 'Monoid' instance. '<>' normalizes -- it removes
-- the creation time that wins from the merged history -- so for an entity whose
-- history repeats its own createdAt, @a <> empty@ would strip that update and
-- differ from @a@. Normalizing at construction and at decoding (see 'normalize')
-- makes such an entity unreachable, but nothing in the type stops one being
-- written down, and an instance asserts a law about every value of the type,
-- not only the reachable ones. 'Semigroup' stays: associativity holds for all
-- of them. See henrytill/hbt-data#38, and #54 for closing the type.
empty :: Entity
empty =
  MkEntity
    { uri = mempty
    , createdAt = mempty
    , updatedAt = mempty
    , names = mempty
    , labels = mempty
    , isFeed = mempty
    , shared = mempty
    , toRead = mempty
    , extended = mempty
    , lastVisitedAt = mempty
    }

mkEntity :: URI -> Time -> Maybe Name -> Set Label -> Entity
mkEntity uri createdAt maybeName labels =
  empty
    { uri
    , createdAt = mkCreatedAt createdAt
    , names = maybe Set.empty Set.singleton maybeName
    , labels
    }

absorb :: Entity -> Entity -> Entity
absorb other existing
  | other /= existing = existing <> other
  | otherwise = existing

nonEmpty :: Text -> Maybe Text
nonEmpty t
  | let stripped = Text.strip t
  , not $ Text.null stripped =
      Just stripped
  | otherwise = Nothing

toLabel :: Text -> Maybe Label
toLabel t = nonEmpty t <&> MkLabel

fromPost :: (HasCallStack) => Post -> IO Entity
fromPost post = do
  uri <- either throwIO pure $ URI.parse post.href
  time <- either throwIO pure $ Time.parseRFC3339 post.time
  let name = post.description >>= nonEmpty <&> MkName
  pure
    MkEntity
      { uri
      , createdAt = mkCreatedAt time
      , updatedAt = Set.empty
      , names = maybe Set.empty Set.singleton name
      , labels = Set.fromList $ Maybe.mapMaybe toLabel post.tags.unTags
      , isFeed = mkIsFeed False
      , shared = mkShared $ Pinboard.toBool post.shared
      , toRead = mkToRead $ Pinboard.toBool post.toread
      , extended = maybe Set.empty (Set.singleton . MkExtended) (post.extended >>= nonEmpty)
      , lastVisitedAt = MkLastVisitedAt Nothing
      }
