module Main (main) where

import Control.Monad (unless)
import Hbt.CollectionTests (results)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let (output, passed) = results
  putStr output
  unless passed exitFailure
