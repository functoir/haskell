module Main where

-- | File: exercise03.hs
--   Purpose: Third  Haskell practice set
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/16/2021



xor :: Bool -> Bool -> Bool
xor c1 c2
  | c1 && c2 = False
  | c1 || c2 = True
  | otherwise = False


mult :: Int -> Int -> Int
mult a b
  | a == 0 || b == 0 = 0
  | otherwise = accmult a b 0
    where
      accmult a 0 c = c
      accmult a b c = accmult a (b-1) (c+a)

maxVal :: (Num a, Ord a) => [a] -> a
maxVal [] = 0
maxVal arr = maxValAcc arr 0
  where
    maxValAcc [] acc = acc
    maxValAcc (x:xs) acc
      | x < acc = maxValAcc xs acc
      | otherwise = maxValAcc xs x

adjpairs :: [a] -> [(a, a)]
adjpairs [] = []
adjpairs [x] =  []
adjpairs (x:y:xs) = (x, y) : adjpairs (y : xs)

mean :: Fractional a => [a] -> a
mean [] = 0
mean arr = findTotal arr / findCount arr
  where
    findTotal :: Fractional a => [a] -> a
    findTotal arr = total arr 0
      where
        total :: Fractional a => [a] -> a -> a
        total [] acc = acc
        total (x:xs) acc = total xs (acc + x)

    findCount :: Fractional a => [a] -> a
    findCount arr = count arr 0
       where
         count [] n = n
         count (x:xs) n = count xs (n+1)

-- Function to find the sum of elements in a list.
sum :: Num a => [a] -> a
sum [] = 0
sum arr = sumAcc arr 0
  where
    sumAcc [] acc = acc
    sumAcc (x:xs) acc = sumAcc xs (acc + x)

-- Function to find the product of elements in a list.
prod :: Num a => [a] -> a
prod [] = 1
prod arr = prodAcc arr 1
  where
    prodAcc [] acc = acc
    prodAcc (x:xs) acc = prodAcc xs (acc * x)

hailstone :: (Num p) => Int -> p
hailstone n
  | n < 1 = 0
  | n == 1 = 1
  | even n = hailstone (n `div` 2)
  | otherwise = hailstone (3*n + 1)

hailstoneAll :: Num p => [Int] -> [p]
hailstoneAll = map hailstone

hailstonePath :: Int -> [Int]
hailstonePath n = compile n []
  where
    compile n acc
      | n < 1 = acc
      | n == 1 = acc ++ [n]
      | even n = compile (n `div` 2) (acc ++ [n])
      | otherwise = compile (3*n + 1) (acc ++ [n])



main :: IO ()
main = do
  putStrLn ""
  putStrLn ""
  print $ "False `xor` False = " ++ show (False `xor` False)
  print $ "True `xor` False = " ++ show (True `xor` False)
  print $ "False `xor` True = " ++ show (False `xor` True)
  print $ "True `xor` True = " ++ show (True `xor` True)
  putStrLn ""
  print $ "0 `mult` 5 = " ++ show (0 `mult` 5)
  print $ "5 `mult` 0 = " ++ show (5 `mult` 0)
  print $ "5 `mult` 5 = " ++ show (5 `mult` 5)
  print $ "5 `mult` 10 = " ++ show (5 `mult` 10)
  print $ "10 `mult` 5 = " ++ show (10 `mult` 5)

  putStrLn ""
  putStrLn ""
  let myArr = [1, 4, 6, 3, 6, 233, 7, 3, 4]
  print $ "array : " ++ show myArr
  print $ "maxVal = " ++ show (maxVal myArr)

  putStrLn ""
  putStrLn ""
  print $ "adjacent pairs : " ++ show (adjpairs myArr)

  putStrLn ""
  putStrLn ""
  let newArr = [1..9]
  print $ "newArr: " ++ show newArr
  print $ "mean: " ++ show (mean newArr)
  putStrLn ""
  putStrLn ""

  print $ "hailstone 5 = " ++ show (hailstone 5)
  print $ "hailstoneAll " ++ show myArr ++ " = " ++ show (hailstoneAll myArr)

  print $ "hailstonePath 3 = " ++ show (hailstonePath 3)
