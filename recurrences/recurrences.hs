module Main where

{------------------- Backward recursion ---------------------}
backwardRev :: [a] -> [a]
backwardRev [] = []
backwardRev (x:xs) = backwardRev xs ++ [x]

{----------------- Tail (forward) recursion -----------------}
tailRev :: [a] -> [a]
tailRev arr = rev' arr []
  where
    rev' [] accumulator = accumulator
    rev' (x:xs) accumulator = rev' xs (x : accumulator)

{--------------------- backward Fib ------------------------}
backwardFib :: Int -> Int
backwardFib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = backwardFib (n-1) + backwardFib (n-2)

{----------------------- Tail Fib ---------------------------}
tailFib :: Int -> Int
tailFib n = pace n 0 1
  where
    pace n first second
      | n == 0 = first
      | otherwise = pace (n - 1) second (first + second)

main :: IO ()
main = do
  -- print "Enter a sentence.\n"
  -- word <- getLine
  -- print word
  let myArr = [1..10]
  putStrLn ""
  putStrLn ""
  print $ "Array: " ++ show myArr
  print $ "backwardRev: " ++ show (backwardRev myArr)
  print $ "tailRev: " ++ show (tailRev myArr)
  putStrLn ""
  print $ "backwardFib 10 = " ++ show (backwardFib 10)
  print $ "tailFib 10 = " ++ show (tailFib 10)

  putStrLn ""
  putStrLn ""