{-# LANGUAGE DeriveAnyClass #-}

module Hbt.Collection.Repr
  ( NodeRepr (..)
  , CollectionRepr (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Hbt.Collection.Id (Id)
import Hbt.Entity (Entity)

data NodeRepr = MkNodeRepr
  { id :: Id
  , entity :: Entity
  , edges :: Vector Id
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CollectionRepr = MkCollectionRepr
  { version :: String
  , length :: Int
  , value :: Vector NodeRepr
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
