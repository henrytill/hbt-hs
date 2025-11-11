module Hbt.Collection.Id where

import Data.Aeson (FromJSON, ToJSON)

newtype Id = MkId {value :: Int}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)
