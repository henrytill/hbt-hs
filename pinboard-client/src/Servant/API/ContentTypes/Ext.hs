{-# LANGUAGE OverloadedStrings #-}

module Servant.API.ContentTypes.Ext where

import Data.Aeson (FromJSON, eitherDecode)
import Network.HTTP.Media ((//))
import Servant.API hiding (JSON)

data JSON

instance Accept JSON where
  contentType _ = "text" // "json"

instance (FromJSON a) => MimeUnrender JSON a where
  mimeUnrender _ = eitherDecode
