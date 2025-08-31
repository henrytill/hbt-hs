module Hbt.Collection.Id
  ( Id (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), parseJSON)

newtype Id = MkId {value :: Int}
  deriving (Show, Eq, Ord)

instance ToJSON Id where
  toJSON (MkId idValue) = toJSON idValue

instance FromJSON Id where
  parseJSON = fmap MkId . parseJSON
