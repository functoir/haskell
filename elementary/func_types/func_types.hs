module Main where

import Prelude hiding (min, max)

inRange :: (Ord a) => a -> a -> a -> Bool
inRange min max x = ilb && iub
  where
    ilb = x <= max
    iub = x >= min

main :: IO ()
main = do 
  print (inRange 10 20 17)
  print (inRange 12 13 12.773)