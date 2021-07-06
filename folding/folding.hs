module Main where

import GHC.Types ( Any )
-- | File: folding.hs
--   Purpose: basic folding in Haskell
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/06/2021

mySum :: (Foldable t, Num b) => p -> t b -> b
mySum arr = foldr (+) 0 

myAND :: [Bool] -> Bool
myAND = foldr (&&) True

myOR :: [Bool] -> Bool
myOR = foldr (||) False

myMax :: (Num p, Ord p) => [p] -> p
myMax [] = 0
myMax [x] = x
myMax (x:xs)
  | x > myMax xs = x
  | otherwise = myMax xs

myMaxFold :: (Num p, Ord p) => [p] -> p
myMaxFold = foldr bigger 0
  where
    bigger :: (Ord p) => p -> p -> p
    bigger num acc
      | num > acc = num
      | otherwise = acc 



isAll :: (Foldable t, Eq a) => a -> t ((Bool -> Bool -> Bool) -> Bool -> a) -> Bool
isAll e = foldr (\x acc -> e==x (&&) acc) True

main :: IO ()
main = do
  let myList = [1, 2, 3, 19, 4, 5, 6]
  print $ myMax myList
  print $ myMaxFold myList
