module Main where

import Data.List ()

-- | File: infinites.hs
--   Purpose: Infinite lists in Haskell.
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/15/2021

{- 
Infinite array of ones
DISCLAIMER -- Generates an infinite list! printing doesn't terminate.
-}
ones :: [Integer]
ones = 1 : ones

generate :: a -> Int -> [a]
generate item count
  | count > 1 =  item : generate item (count - 1)
  | otherwise = []

fibRec :: Int -> Int
fibRec n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibRec (n - 1) + fibRec (n - 2)

fibDynamic :: Int -> Integer
fibDynamic n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- fibDynamic2 :: Int -> Int
-- fibDynamic2 n = find 0 1 n
--   where 
--     find a b step = do
--       while step > 1 do
--       | step <= 1 = step
--       | otherwise = do
--         temp <- a + b
--         a <- b
--         b <- temp
--         count <- step - 1
--         return b


main :: IO ()
main = do
  -- print ones
  print $ generate 0.77 5
  print $ generate 'a' 5
  print $ "0" ++ " -> " ++ show (fibRec 0) ++" " ++ show (fibDynamic 0)
  print $ "1" ++ " -> " ++ show (fibRec 1) ++" " ++ show (fibDynamic 1)
  print $ "2" ++ " -> " ++ show (fibRec 2) ++" " ++ show (fibDynamic 2)
  print $ "3" ++ " -> " ++ show (fibRec 3) ++" " ++ show (fibDynamic 3)
  print $ "4" ++ " -> " ++ show (fibRec 4) ++" " ++ show (fibDynamic 4)
  print $ "5" ++ " -> " ++ show (fibRec 5) ++" " ++ show (fibDynamic 5)
