{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hbt.Parser.Common
  ( -- * IsEmpty type class and Empty pattern
    IsEmpty (..)
  , pattern Empty

    -- * StateIO
  , StateIO
  , runStateIO

    -- * Misc
  , drop1
  )
where

import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Entity.URI (URI)
import Hbt.Entity.URI qualified as URI

class IsEmpty s where
  isEmpty :: s -> Bool
  empty :: s

instance IsEmpty Text where
  isEmpty = Text.null
  empty = Text.empty

instance IsEmpty ByteString where
  isEmpty = ByteString.null
  empty = ByteString.empty

instance IsEmpty URI where
  isEmpty = URI.null
  empty = URI.empty

pattern Empty :: (IsEmpty s) => s
pattern Empty <- (isEmpty -> True)
  where
    Empty = empty

type StateIO s = StateT s IO

runStateIO :: StateIO s a -> s -> IO (a, s)
runStateIO = State.runStateT

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs
