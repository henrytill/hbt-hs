module Hbt.Collection.Id where

import Data.Aeson (FromJSON (..), ToJSON (..))

newtype Id = MkId {value :: Int}
  deriving (Show, Eq, Ord)

instance ToJSON Id where
  toJSON (MkId idValue) = toJSON idValue

instance FromJSON Id where
  parseJSON json = fmap MkId (parseJSON json)
