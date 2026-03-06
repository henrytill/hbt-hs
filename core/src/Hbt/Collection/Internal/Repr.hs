{-# LANGUAGE DeriveAnyClass #-}

module Hbt.Collection.Internal.Repr
  ( NodeRepr (..)
  , CollectionRepr (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Hbt.Entity (Entity)

data NodeRepr = MkNodeRepr
  { id :: Int
  , entity :: Entity
  , edges :: Vector Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CollectionRepr = MkCollectionRepr
  { version :: String
  , length :: Int
  , value :: Vector NodeRepr
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
