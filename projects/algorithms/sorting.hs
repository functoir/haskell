{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq ( force )

-- | Check if an index is within bounds of an array.
outOfBounds :: Int -> [a] -> Bool
outOfBounds index arr = force index >= length arr

-- | Swap two elements in an array.
swap :: Ord a => Int -> Int -> [a] -> [a]
swap i j arr
  | null arr = arr
  | i == j || outOfBounds i arr || outOfBounds j arr = arr
  | otherwise =
    let !lo = min i j ; hi = max i j
        !left = take lo arr
        !middle = drop (lo + 1) $! take hi arr
        !right = drop (hi + 1) arr
    in left ++ [arr !! hi] ++ middle ++ [arr !! lo] ++ right


bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort arr = loop arr 0 0 0
  where
    loop :: (Ord a ) => [a] -> Int -> Int -> Int -> [a]
    loop arr i step swaps
      | step == length arr = arr
      | i >= length arr - 1 = if swaps > 0 then loop arr 0 (step + 1) 0 else arr
      | otherwise =
        if arr !! i > arr !! (i + 1)
          then loop (swap i (i + 1) arr) (i + 1) step (swaps + 1)
        else loop arr (i + 1) step swaps


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort arr = merge (mergeSort $! first arr) (mergeSort $! second arr)
  where
    !half= length arr `div` 2
    first array = force take half array
    second array = force drop half array
    merge [] [] = []
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x < y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys



main :: IO ()
main = do
  let myArr = [0, 1, 2, 3, 4, 5]
  print $! swap 1 4 myArr

  let arr2 = [5, 4, 3, 2, 1, 0]
  print $ "array = " ++ show arr2
  print $ "bubble-sorted = " ++ show (bubbleSort arr2)
  print $ "merge-sorted = " ++ show (mergeSort arr2)
  print $ "bubble-sorted = " ++ show (bubbleSort (reverse [1..20]))
  print $ "merge-sorted = " ++ show (mergeSort (reverse [1..20]))
  print $ "bubble-sorted = " ++ show (bubbleSort [0, 5, 4, 6, 7, 8])
  print $ "merge-sorted = " ++ show (mergeSort [0, 5, 4, 6, 7, 8])
  -- print $ "sorted = " ++ show (bubbleSort [])
  print $ "bubble-sorted = " ++ show (bubbleSort [1])
  print $ "merge-sorted = " ++ show (mergeSort [1])