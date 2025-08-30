{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Common where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.HTML.TagSoup (Attribute)

lookupAttr :: Text -> [Attribute Text] -> Maybe Text
lookupAttr key attrs = lookup (Text.toLower key) (map (first Text.toLower) attrs)
