{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

main :: IO ()
main = do
  print $ take 20 primes
  print $ primesInRange 20 50
  -- print $ take 1000 primes
  -- print $ primesInRange 1000 2000
  print $ primesInRange 10000 10100


primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primesInRange :: Int -> Int -> [Int]
primesInRange a b =
  filter (>= a) $ takeWhile (<= b) primes
