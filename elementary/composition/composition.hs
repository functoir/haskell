module Main where

import Prelude hiding (flip)

myDot :: (b -> c) -> (a -> b) -> a -> c
myDot f g x = f (g x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


myRevFold :: [a] -> [a]
myRevFold = foldr prepend []
  where
    prepend :: a -> [a] -> [a]
    prepend num arr = arr ++ [num]

-- | Function to sort an array
mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort [x] = [x]
mySort (x:y:xs)
  | x > y = flip (y : mySort (x : xs))
  | otherwise = flip (x : mySort (y : xs))
  where
    -- | Function to flip the first two elements in an array
    --   and re-sort the array if needed
    flip :: Ord t => [t] -> [t]
    flip [] = []
    flip [x] = [x]
    flip (x : y : xs)
      | x > y = mySort (y : x : xs)
      | otherwise = x : y : xs


flip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip func x y = func y x

divide :: Double -> Double -> Double
divide = flip (/)

sumCubes :: Num a => [a] -> a
sumCubes arr = compile arr 0
  where
    compile [] sum = sum
    compile (x:xs) sum = compile xs (sum + x^3)
      

main :: IO ()
main = do
  let myArr = [ 12, 13, 1, 17, 234, 6, 483924, 534, 3, 2, 15, 7, 0, 22, 3]

  putStr "array = "
  print myArr
  putStr "sorted array = "
  print $ mySort myArr
  let revSort = myDot mySort myReverse
  print $ revSort myArr
  let revSort2 = mySort . myReverse
  print $ revSort2 myArr


  putStr "reversed array = "
  print $ myReverse myArr
  putStr "myRevFold: "
  print $ myRevFold myArr

  putStr "sorted and reversed array = "
  let sortRev = myDot myReverse mySort
  print $ sortRev myArr

  print $ divide 2 3

  let newArr = [1, 2, 3, 4, 5]
  print $ sumCubes newArr
