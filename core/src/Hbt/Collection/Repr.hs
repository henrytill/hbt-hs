module Hbt.Collection.Repr
  ( NodeRepr (..)
  , CollectionRepr (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
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

instance ToJSON NodeRepr where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON NodeRepr where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

data CollectionRepr = MkCollectionRepr
  { version :: String
  , length :: Int
  , value :: Vector NodeRepr
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CollectionRepr where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON CollectionRepr where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
