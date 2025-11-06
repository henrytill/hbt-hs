module Main where

import Pinboard.Client qualified (someFunc)

main :: IO ()
main = do
  Pinboard.Client.someFunc
