module Hbt.Parser.Common
  ( StateIO
  , runStateIO
  , drop1
  )
where

import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State

type StateIO s = StateT s IO

runStateIO :: StateIO s a -> s -> IO (a, s)
runStateIO = State.runStateT

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs
