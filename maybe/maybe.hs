module Main where

import Data.Maybe (isJust, isNothing, fromJust)

-- | File: maybe.hs
--   Purpose: `Maybe` data types in Haskell
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/15/2021

safediv :: Integral a => a -> a -> Maybe a
safediv a b
  | b == 0 = Nothing
  | otherwise = Just $ a `div` b


main :: IO ()
main = do
  print $ safediv 10 2
  print $ safediv 10 0
