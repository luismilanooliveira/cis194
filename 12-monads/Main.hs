module Main where

import Risk
import Control.Monad
import Control.Monad.Random


main = do
  match <- evalRandIO $ successProb (Battlefield 5 8)
  print match
