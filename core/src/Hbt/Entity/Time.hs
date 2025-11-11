module Hbt.Entity.Time
  ( Error
  , Time
  , toText
  , epoch
  , fromSeconds
  , parse
  , parseRFC3339
  , parseTimestamp
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Read
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format

data Error
  = InvalidTime Text
  deriving stock (Show, Eq)

instance Exception Error

newtype Time = MkTime {unTime :: POSIXTime}
  deriving stock (Show, Eq, Ord)

toText :: Time -> Text
toText (MkTime posixTime) = Text.pack (show @Integer (round posixTime))

instance ToJSON Time where
  toJSON = toJSON . toText

instance FromJSON Time where
  parseJSON json = fmap (MkTime . fromInteger) (parseJSON json)

epoch :: Time
epoch = MkTime 0

fromSeconds :: Int -> Time
fromSeconds = MkTime . fromIntegral

parse :: Text -> Either Error Time
parse s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%B %e, %Y" (Text.unpack s) of
    Nothing -> Left (InvalidTime s)
    Just utcTime -> Right (MkTime (POSIX.utcTimeToPOSIXSeconds utcTime))

parseRFC3339 :: Text -> Either Error Time
parseRFC3339 s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack s) of
    Nothing -> Left (InvalidTime s)
    Just utcTime -> Right (MkTime (POSIX.utcTimeToPOSIXSeconds utcTime))

parseTimestamp :: Text -> Maybe Time
parseTimestamp s =
  case Read.decimal s of
    Left {} -> Nothing
    Right (i, rest) | Text.null rest -> Just (MkTime (fromInteger i))
    Right {} -> Nothing
