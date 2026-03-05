{-# LANGUAGE RoleAnnotations #-}

module Hbt.Collection.Id (Id (..)) where

type role Id nominal

newtype Id s = MkId {value :: Int}
  deriving stock (Eq, Ord, Show)
