{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pinboard.Client (someFunc) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Network.TLS (ClientParams (..), EMSMode (AllowEMS), Supported (..))
import Servant.API hiding (JSON)
import Servant.API.ContentTypes.Ext
import Servant.Client
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Directory
import System.Exit qualified as Exit
import System.FilePath ((</>))

type PostsUpdate =
  "posts"
    :> "update"
    :> QueryParam "format" Format
    :> QueryParam "auth_token" ApiToken
    :> Get '[JSON] UpdateTime

type PostsGet =
  "posts"
    :> "get"
    :> QueryParam "format" Format
    :> QueryParam "auth_token" ApiToken
    :> QueryParam "tag" Tag
    :> Get '[JSON] [Bookmark]

type API = PostsUpdate :<|> PostsGet

api :: Proxy API
api = Proxy

-- | Returns the most recent time a bookmark was added, updated or deleted.
update :: Maybe Format -> Maybe ApiToken -> ClientM UpdateTime

-- | Returns one or more posts on a single day matching the arguments.
--
-- If no date or url is given, date of most recent bookmark will be used.
get :: Maybe Format -> Maybe ApiToken -> Maybe Tag -> ClientM [Bookmark]
update :<|> get = client api

newtype UpdateTime = MkUpdateTime {unUpdateTime :: UTCTime}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

remapField :: [(String, String)] -> String -> String
remapField mappings field
  | Just mapped <- lookup field mappings = mapped
  | otherwise = field

updateTimeOptions :: Options
updateTimeOptions = Aeson.defaultOptions {fieldLabelModifier}
  where
    mappings :: [(String, String)]
    mappings = [("unUpdateTime", "update_time")]
    fieldLabelModifier = remapField mappings

data Bookmark = MkBookmark
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Tag = MkTag {unTag :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData)

data Format = JSON
  deriving stock (Eq, Show)

instance ToHttpApiData Format where
  toQueryParam JSON = "json"

newtype ConfigVersion = MkConfigVersion {unConfigVersion :: Int}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype ApiToken = MkApiToken {unApiToken :: Text}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, ToHttpApiData)

data Config = MkConfig
  { version :: ConfigVersion
  , apiToken :: ApiToken
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

findConfigFile :: IO (Maybe FilePath)
findConfigFile = do
  configDir <- Directory.getXdgDirectory XdgConfig "hbt"
  let configFile = configDir </> "hbt.json"
  configExists <- Directory.doesFileExist configFile
  pure $
    if configExists
      then Just configFile
      else Nothing

readConfig :: FilePath -> IO Config
readConfig path = do
  configText <- Text.IO.readFile path
  Aeson.throwDecodeStrictText configText

managerSettings :: ManagerSettings
managerSettings = TLS.mkManagerSettings tlsSettings Nothing
  where
    settingClientSupported = (def @Supported) {supportedExtendedMainSecret = AllowEMS}
    tlsSettings = case def @TLSSettings of
      settings@(TLSSettingsSimple {}) -> settings {settingClientSupported}
      TLSSettings params -> TLSSettings (params {clientSupported = settingClientSupported})

runClient :: ApiToken -> IO ()
runClient apiToken = do
  manager <- Client.newManager managerSettings
  let m = update (Just JSON) (Just apiToken)
      env = mkClientEnv manager $ BaseUrl Https "api.pinboard.in" 443 "v1"
  result <- runClientM m env
  response <- either throwIO pure result
  putStrLn $ "updated: " <> show response

someFunc :: IO ()
someFunc = do
  maybeConfigFile <- findConfigFile
  case maybeConfigFile of
    Nothing -> Exit.die "no config found"
    Just configFile -> do
      config <- readConfig configFile
      putStrLn $ "config: " <> show config
      runClient config.apiToken
