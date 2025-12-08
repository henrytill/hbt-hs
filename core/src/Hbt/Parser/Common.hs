module Hbt.Parser.Common
  ( drop1
  )
where

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs
