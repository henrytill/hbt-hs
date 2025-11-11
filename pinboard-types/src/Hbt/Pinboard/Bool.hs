{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hbt.Pinboard.Bool
  ( Bool
  , pattern True
  , pattern False
  , toBool
  )
where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (Bool, False, True)
import Prelude qualified

newtype Bool = MkBool Text
  deriving stock (Show, Eq)

pattern True :: Bool
pattern True = MkBool "yes"

pattern False :: Bool
pattern False = MkBool "no"

{-# COMPLETE True, False #-}

mkBool :: (MonadFail m) => Text -> m Bool
mkBool "yes" = pure True
mkBool "no" = pure False
mkBool t = fail ("expected 'yes' or 'no', got: " <> Text.unpack t)

instance FromJSON Bool where
  parseJSON = Aeson.withText "Hbt.Pinboard.Bool" mkBool

toBool :: Bool -> Prelude.Bool
toBool True = Prelude.True
toBool False = Prelude.False
