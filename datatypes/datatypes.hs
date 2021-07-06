module Main where

data Calculation =
  Add Int Int | Sub Int Int | Mul Int Int | Div Int Int | Sum [Int] Int
calc :: Calculation -> Int
calc $ Add x y = x + y
calc $ Sub x y = x - y
calc $ Mul x y = x * y
calc $ Div x y = x / y
calc $ Sum (x:xs) = x + Sum xs

main :: IO ()
main = do
  let newCal  = Add 5 42
  print newCal