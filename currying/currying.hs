module Main where

-- The following are equivalent functions
addV1 :: Int -> Int -> Int
addV1 x y = x + y

addV2 :: Num a => a -> a -> a
addV2 x = (\y -> x + y)

add :: Int -> Int -> Int
add = (\x -> (\y -> x + y))




main :: IO ()
main = do
  print (addV1 1 2)
  print (addV2 1 2)
  print (add 1 2)

  -- Partial function application
  let add1 = add 1
  let add5 = add 5
  print (add1 2)
  print (add5 2)
  