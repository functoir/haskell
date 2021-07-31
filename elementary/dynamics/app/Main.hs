module Main where


fib :: Int -> Integer
fib n = fibsDp !! (n - 1)

fibsDp :: [Integer]
fibsDp = 1 : 1 : zipWith (+) fibsDp (tail fibsDp)
-- fibsDp = 1 : 1 : [fibsDp !! (n-1) + fibsDp !! (n-2) | n <- [2..]]


main :: IO ()
main = do
  print $ show (take 3 fibsDp)
  print $ show (fib 99)
  print $ show (fib 100)
  print $ show (fib 101)
  print $ show (fib 102)
  print $ show (take 15 fibsDp)
