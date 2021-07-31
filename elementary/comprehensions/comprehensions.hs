module Main where

import Test.QuickCheck (quickCheck)

prop :: Ord a => [a] -> Bool
prop xs = length xs == length (quicksort xs) 




isPrime :: Int -> Bool
isPrime n = null (factors n)
  where
    factors m = [x | x <- [2..(m-1)], m `mod` x == 0]

double :: Num a => [a] -> [a]
double xs = [x * 2 | x <- xs]

positions :: Eq a => [a] -> a -> [Int]
positions xs n = [i | (i, x) <- zip [1..length xs] xs, x == n]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

main :: IO ()
main = do
  putStr "\n\n"

  print $ isPrime 17
  print $ "doubles of : " ++ show [-4..4] ++ " = " ++ show (double [-4..4])

  print $ "occurrences of 5 in [1, 2, 3, 5, 7, 5, 3, 5, 5] : " ++ show (positions [1, 2, 3, 5, 7, 5, 3, 5, 5] 5)

  putStr "\n\n"

  let myArr = [2, 4, 2, 5, 3, 4, 3, 24 , 5, 34 , 3, 4,3, 4, 3, 24, 4, 32, 4, 234, 23, 4, 23, 5, 354, 23, 4, 234, 34, 342]
  print $ show myArr
  print $ "sorted : " ++ show (quicksort myArr)

  