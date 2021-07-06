module Main where
-- Higher Order Functions: functions that take other functions as input

-- Apply a function as is
app :: (t1 -> t2) -> t1 -> t2
app f = f

-- Propagate a function through an array (similar to `map`)
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) =
  f x : myMap f xs

-- Filter items from an array given a type check

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x : xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs



main :: IO ()
main = do
  print (app ( +1) 1)
  let myArr1 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  print (myMap (*10) myArr1)
  print (myFilter even myArr1)


