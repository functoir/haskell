-- {-# LANGUAGE ViewPatterns #-}

module Main where

-- |  Compute, for each index,
-- the product of every other element in array.
prod :: [Int] -> [Int]
prod arr =
  [productOfAll `div` x | x <- arr, x /= 0]
  where
    productOfAll = product arr

prod2 :: [Int] -> [Int]
prod2 arr =
  lscan arr (rscan arr []) []
  where
    lscan :: [Int] -> [Int] -> [Int] -> [Int]
    lscan [] _ acc = acc
    lscan _ [] _ = []
    lscan (x:xs) (y:ys) acc = acc ++ [last acc * x]

    rscan :: [Int] -> [Int] -> [Int]
    rscan [] acc = acc
    rscan (x:xs) acc = acc ++ [last acc * x]


prod3 :: [Int] -> [Int]
prod3 arr =
  loop arr []
  where
    loop :: [Int] -> [Int] -> [Int]
    loop [] acc = acc
    loop (x:xs) acc = loop xs (acc ++ [last acc * x])



main :: IO ()
main = do
  let arr = [5..8]
  print arr
  print $ prod arr

  let arr = [1..5]
  print arr
  print $ prod arr

  let arr = [0..5]
  print arr
  print $ prod arr


  return ()
