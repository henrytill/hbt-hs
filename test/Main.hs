module Main (main) where

import Control.Monad (unless)
import Hbt.CollectionTest (results)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let (output, passed) = results
  putStr output
  unless passed exitFailure
