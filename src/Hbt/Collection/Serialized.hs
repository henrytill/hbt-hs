module Hbt.Collection.Serialized where

import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Hbt.Collection.Id (Id)
import Hbt.Entity (Entity)

data SerializedNode = MkSerializedNode
  { id :: Id
  , entity :: Entity
  , edges :: Vector Id
  }
  deriving (Show, Eq, Generic)

instance ToJSON SerializedNode

instance FromJSON SerializedNode

data SerializedCollection = MkSerializedCollection
  { version :: String
  , length :: Int
  , value :: Vector SerializedNode
  }
  deriving (Show, Eq, Generic)

instance ToJSON SerializedCollection

instance FromJSON SerializedCollection
