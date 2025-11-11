{-# LANGUAGE OverloadedStrings #-}

module Pinboard.Client where

import Data.Aeson (FromJSON (..), Options, ToJSON (..))
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

remapField :: [(String, String)] -> String -> String
remapField mappings field
  | Just mapped <- lookup field mappings = mapped
  | otherwise = field

newtype UpdateTime = MkUpdateTime {unUpdateTime :: UTCTime}
  deriving stock (Eq, Show, Generic)

updateTimeOptions :: Options
updateTimeOptions =
  let mappings = [("unUpdateTime", "update_time")]
   in Aeson.defaultOptions {Aeson.fieldLabelModifier = remapField mappings}

instance ToJSON UpdateTime where
  toJSON = Aeson.genericToJSON updateTimeOptions

instance FromJSON UpdateTime where
  parseJSON = Aeson.genericParseJSON updateTimeOptions

data Bookmark = MkBookmark
  deriving stock (Eq, Show, Generic)

instance ToJSON Bookmark where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON Bookmark where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

newtype Tag = MkTag {unTag :: Text}
  deriving stock (Eq, Show)

instance ToHttpApiData Tag where
  toQueryParam (MkTag t) = t

data Format = JSON
  deriving stock (Eq, Show)

instance ToHttpApiData Format where
  toQueryParam JSON = "json"

newtype ConfigVersion = MkConfigVersion {unConfigVersion :: Int}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

newtype ApiToken = MkApiToken {unApiToken :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

data Config = MkConfig
  { version :: ConfigVersion
  , apiToken :: ApiToken
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Config where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON Config where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

readConfig :: FilePath -> IO Config
readConfig path = do
  configText <- Text.IO.readFile path
  Aeson.throwDecodeStrictText configText

managerSettings :: ManagerSettings
managerSettings =
  let settingClientSupported = (def @Supported) {supportedExtendedMainSecret = AllowEMS}
      tlsSettings = case def @TLSSettings of
        settings@(TLSSettingsSimple {}) -> settings {settingClientSupported}
        TLSSettings params -> TLSSettings (params {clientSupported = settingClientSupported})
   in TLS.mkManagerSettings tlsSettings Nothing

runClient :: ApiToken -> IO ()
runClient apiToken = do
  manager <- Client.newManager managerSettings
  let m = update (Just JSON) (Just apiToken)
      env = mkClientEnv manager (BaseUrl Https "api.pinboard.in" 443 "v1")
  result <- runClientM m env
  case result of
    Left err -> error (show err)
    Right response -> putStrLn ("updated: " ++ show response)

someFunc :: IO ()
someFunc = do
  configDir <- Directory.getXdgDirectory XdgConfig "hbt"
  let configFile = configDir ++ "/hbt.json"
  putStrLn ("config: " ++ configFile)
  configExists <- Directory.doesFileExist configFile
  if not configExists
    then error "config file does not exist"
    else do
      config <- readConfig configFile
      putStrLn ("parsed: " ++ show config)
      runClient config.apiToken
