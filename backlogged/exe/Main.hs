module Main where

import Backlogged qualified (someFunc)

main :: IO ()
main = do
  Backlogged.someFunc
