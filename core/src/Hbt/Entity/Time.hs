{-# LANGUAGE DeriveAnyClass #-}

module Hbt.Entity.Time
  ( Error
  , Time
  , toText
  , toSeconds
  , epoch
  , fromSeconds
  , parse
  , parseRFC3339
  , parseTimestamp
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Read
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format qualified as Format

newtype Error = InvalidTime Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype Time = MkTime {unTime :: POSIXTime}
  deriving stock (Eq, Ord, Show)

fromSeconds :: Int64 -> Time
fromSeconds = MkTime . realToFrac @Int64

epoch :: Time
epoch = fromSeconds 0

instance Bounded Time where
  minBound = epoch
  maxBound = fromSeconds maxBound

toSeconds :: Time -> Int64
toSeconds (MkTime posixTime) = round posixTime

toText :: Time -> Text
toText = Text.pack . show . toSeconds

-- | The shared wire format spells a time as an integer, not as a string: the
-- schema's Time is {"type": "integer", "format": "int64"}, and the fixtures
-- write @createdAt: 1609459200@ unquoted. Writing it through 'toText' produced
-- a quoted string that the matching 'FromJSON' then refused to read back.
instance ToJSON Time where
  toJSON = toJSON . toSeconds

instance FromJSON Time where
  parseJSON = fmap fromSeconds . parseJSON

parse :: Text -> Either Error Time
parse s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%B %e, %Y" (Text.unpack s) of
    Nothing -> Left $ InvalidTime s
    Just utcTime -> Right . MkTime $ POSIX.utcTimeToPOSIXSeconds utcTime

parseRFC3339 :: Text -> Either Error Time
parseRFC3339 s =
  case Format.parseTimeM @Maybe True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack s) of
    Nothing -> Left $ InvalidTime s
    Just utcTime -> Right . MkTime $ POSIX.utcTimeToPOSIXSeconds utcTime

parseTimestamp :: Text -> Maybe Time
parseTimestamp s =
  case Read.decimal s of
    Left {} -> Nothing
    Right (i, rest) | Text.null rest -> Just . MkTime $ fromInteger i
    Right {} -> Nothing
