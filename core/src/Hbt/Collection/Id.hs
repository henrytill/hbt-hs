module Hbt.Collection.Id (Id (..)) where

import Data.Aeson (FromJSON, ToJSON)

newtype Id = MkId {value :: Int}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)
