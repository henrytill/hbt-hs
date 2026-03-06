{-# LANGUAGE RoleAnnotations #-}

module Hbt.Collection.Internal.Id (Id (..)) where

type role Id nominal

newtype Id s = MkId {value :: Int}
  deriving stock (Eq, Ord, Show)
