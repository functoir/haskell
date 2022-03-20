{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}


module SortLib (
  doSomething,
  mergesort,
  quicksort,
  bubblesort,
  selectionsort
) where

import Prelude

doSomething :: Int
doSomething = 1

{- |
`merge-sort` an array of Ordinal values.

List items must have `Ord` instance.

__Time complexity: O(n log n)__

__examples__:
  
  @mergesort [5,4,3,2,1] == [1,2,3,4,5]@

  To get a reversed sorting, consider composing `reverse` with `mergesort`:

  @reversedSorting = reverse . mergesort@
-}
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort arr = merge (mergesort left) (mergesort right)
    where
      (left, right) = splitAt (length arr `div` 2) arr
      merge :: Ord a => [a] -> [a] -> [a]
      merge [] ys = ys    -- [x] =========> (x:[])
      merge xs [] = xs
      merge (x:xs) (y:ys)
        | x < y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

{- |
`quick-sort` an array of Ordinal values.

List items must have `Ord` instance.

__Time complexity: O(n^2)__

__examples__:
  
  @quicksort [5,4,3,2,1] == [1,2,3,4,5]@

  To get a reversed sorting, consider composing `reverse` with `quicksort`:
  
  @reversedSorting = reverse . quicksort@
-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =                --(x:[])
  smallerSorted ++ [x] ++ biggerSorted
  where
    smallerSorted = quicksort [y | y <- xs, y <= x]
    biggerSorted = quicksort [y | y <- xs, y > x]

{- |
`bubble-sort` an array of Ordinal values.

List items must have `Ord` instance.

__Time complexity: O(n^2)__

__examples__:
  
  @bubblesort [5,4,3,2,1] == [1,2,3,4,5]@

  To get a reversed sorting, consider composing `reverse` with `bubblesort`:
  
  @reversedSorting = reverse . bubblesort@
-}
bubblesort :: Ord a => [a] -> [a]
bubblesort arr = iter n arr
  where
    n = length arr
    iter :: Ord a => Int -> [a] -> [a]
    iter 0 xs = xs
    iter _ [] = []
    iter _ [x] = [x]
    iter step xs = iter (step-1) $ swap xs
      where
        swap [] = []
        swap [x] = [x]
        swap (x:y:xs)
          | x > y = y : swap (x:xs)
          | otherwise = x : swap (y:xs)


{- |
`selection-sort` an array of Ordinal values.

List items must have `Ord` instance.

__Time complexity: O(n^2)__

__examples__:
  
  @selectionsort [5,4,3,2,1] == [1,2,3,4,5]@

  To get a reversed sorting, consider composing `reverse` with `selectionsort`:
  
  @reversedSorting = reverse . selectionsort@
-}
selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort arr = minItem : selectionsort (remove minIndex arr)
  where
    (minItem, minIndex) = minimum $ map (\x -> (arr !! x, x)) [0..(length arr - 1)]
    remove index xs = left ++ tail right
      where
        (left, right) = splitAt index xs

-- type Bin = [Int]
-- type Bins = [Bin]
-- radixsort :: (Num a, Ord a, Show a) => [a] -> [a]
-- radixsort [] = []
-- radixsort arr = iter steps arr [[]]
--   where
--     steps = length (show $ maximum arr)
--     iter :: Num a => Int -> [a] -> Bins -> [a]
--     iter 0 xs _ = xs
--     iter 
  


