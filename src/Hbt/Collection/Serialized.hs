module Hbt.Collection.Serialized where

import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Hbt.Collection.Entity (Entity)
import Hbt.Collection.Id (Id)

data SerializedNode = SerializedNode
  { id :: Id
  , entity :: Entity
  , edges :: Vector Id
  }
  deriving (Show, Eq, Generic)

instance ToJSON SerializedNode

instance FromJSON SerializedNode

data SerializedCollection = SerializedCollection
  { version :: String
  , length :: Int
  , value :: Vector SerializedNode
  }
  deriving (Show, Eq, Generic)

instance ToJSON SerializedCollection

instance FromJSON SerializedCollection
