module Hbt.Parser.Common
  ( drop1
  , uses
  )
where

import Control.Monad.State.Class (MonadState)
import Lens.Micro (Getting)
import Lens.Micro.Mtl (use)

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs

uses :: (MonadState s m) => Getting a s a -> (a -> r) -> m r
uses l f = f <$> use l
