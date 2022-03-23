{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Prelude
import SortLib (
  mergesort, 
  quicksort, 
  bubblesort,
  selectionsort
  )

main :: IO ()
main = do

  let xs = reverse [1,2,3,4,5,6,7,8,9,10]
  let emptyArr = [] :: [Int]
  let singleton = [2]
  let duo = [10, 9]
  let trio = [10, 9, 8]
  -- let long = [1000, 999 .. 0]

  putStrLn $ "array:         " ++ show xs
  putStrLn $ "mergesort:     " ++ show (mergesort xs)
  putStrLn $ "quicksort:     " ++ show (quicksort xs)
  putStrLn $ "bubblesort:    " ++ show (bubblesort xs)
  putStrLn $ "selectionsort: " ++ show (selectionsort xs)
  putStrLn ""

  putStrLn $ "array:         " ++ show emptyArr
  putStrLn $ "mergesort:     " ++ show (mergesort emptyArr)
  putStrLn $ "quicksort:     " ++ show (quicksort emptyArr)
  putStrLn $ "bubblesort:    " ++ show (bubblesort emptyArr)
  putStrLn $ "selectionsort: " ++ show (selectionsort emptyArr)
  putStrLn ""

  putStrLn $ "array:         " ++ show singleton
  putStrLn $ "mergesort:     " ++ show (mergesort singleton)
  putStrLn $ "quicksort:     " ++ show (quicksort singleton)
  putStrLn $ "bubblesort:    " ++ show (bubblesort singleton)
  putStrLn $ "selectionsort: " ++ show (selectionsort singleton)
  putStrLn ""

  putStrLn $ "array:         " ++ show duo
  putStrLn $ "mergesort:     " ++ show (mergesort duo)
  putStrLn $ "quicksort:     " ++ show (quicksort duo)
  putStrLn $ "bubblesort:    " ++ show (bubblesort duo)
  putStrLn $ "selectionsort: " ++ show (selectionsort duo)
  putStrLn ""

  putStrLn $ "array:         " ++ show trio
  putStrLn $ "mergesort:     " ++ show (mergesort trio)
  putStrLn $ "quicksort:     " ++ show (quicksort trio)
  putStrLn $ "bubblesort:    " ++ show (bubblesort trio)
  putStrLn $ "selectionsort: " ++ show (selectionsort trio)
  putStrLn ""

  -- putStrLn $ "array:         " ++ show long
  -- putStrLn $ "mergesort:     " ++ show (mergesort long)
  -- putStrLn $ "quicksort:     " ++ show (quicksort long)
  -- putStrLn $ "bubblesort:    " ++ show (bubblesort long)
  -- putStrLn $ "selectionsort: " ++ show (selectionsort long)
  -- putStrLn ""
