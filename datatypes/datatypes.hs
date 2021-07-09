module Main where

data Calculation =
  Add Int Int | Sub Int Int | Mul Int Int | Div Int Int | Sum [Int]



calc :: Calculation -> Int
calc (x `Add` y) = x + y
calc (x `Sub` y) = x - y
calc (x `Mul` y) = x * y
calc (x `Div` y) = x `div` y
calc (Sum arr) = Main.sum arr

sum :: [Int] -> Int
sum = foldr (+) 0

main :: IO ()
main = do
  let newCal  = 5 `Add` 42
  print $ calc newCal

  let newCal = 27 `Div` 7
  print $ calc newCal

  let myArr = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  let newCalc = Sum myArr
  print $ calc newCalc