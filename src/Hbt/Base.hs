{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeData #-}

module Hbt.Base
  ( -- * Flow and Format Types
    Flow (..)
  , Format (..)
  , InputFormat
  , OutputFormat
  )
where

type data Flow = From | To

data Format (f :: Flow) where
  JSON :: Format From
  XML :: Format From
  Markdown :: Format From
  HTML :: Format f
  YAML :: Format To

deriving instance Show (Format f)

deriving instance Eq (Format f)

type InputFormat = Format From

type OutputFormat = Format To
