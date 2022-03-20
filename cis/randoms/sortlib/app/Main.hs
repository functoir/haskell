{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Prelude
import SortLib (
  doSomething, 
  mergesort, 
  quicksort, 
  bubblesort,
  selectionsort
  )

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print doSomething

  let xs = reverse [1,2,3,4,5,6,7,8,9,10]
  let emptyArr = [] :: [Int]
  let singleton = [2]
  let duo = [10, 9]
  let trio = [10, 9, 8]

  print $ "array:         " ++ show xs
  print $ "mergesort:     " ++ show (mergesort xs)
  print $ "quicksort:     " ++ show (quicksort xs)
  print $ "bubblesort:    " ++ show (bubblesort xs)
  print $ "selectionsort: " ++ show (selectionsort xs)
  putStrLn ""

  print $ "array:         " ++ show emptyArr
  print $ "mergesort:     " ++ show (mergesort emptyArr)
  print $ "quicksort:     " ++ show (quicksort emptyArr)
  print $ "bubblesort:    " ++ show (bubblesort emptyArr)
  print $ "selectionsort: " ++ show (selectionsort emptyArr)
  putStrLn ""

  print $ "array:         " ++ show singleton
  print $ "mergesort:     " ++ show (mergesort singleton)
  print $ "quicksort:     " ++ show (quicksort singleton)
  print $ "bubblesort:    " ++ show (bubblesort singleton)
  print $ "selectionsort: " ++ show (selectionsort singleton)
  putStrLn ""

  print $ "array:         " ++ show duo
  print $ "mergesort:     " ++ show (mergesort duo)
  print $ "quicksort:     " ++ show (quicksort duo)
  print $ "bubblesort:    " ++ show (bubblesort duo)
  print $ "selectionsort: " ++ show (selectionsort duo)
  putStrLn ""

  print $ "array:         " ++ show trio
  print $ "mergesort:     " ++ show (mergesort trio)
  print $ "quicksort:     " ++ show (quicksort trio)
  print $ "bubblesort:    " ++ show (bubblesort trio)
  print $ "selectionsort: " ++ show (selectionsort trio)
  putStrLn ""
