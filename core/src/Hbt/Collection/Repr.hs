module Hbt.Collection.Repr where

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
  deriving (Show, Eq, Generic)

instance ToJSON NodeRepr

instance FromJSON NodeRepr

data CollectionRepr = MkCollectionRepr
  { version :: String
  , length :: Int
  , value :: Vector NodeRepr
  }
  deriving (Show, Eq, Generic)

instance ToJSON CollectionRepr

instance FromJSON CollectionRepr
