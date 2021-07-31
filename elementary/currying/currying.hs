module Main where

-- The following are equivalent functions
addV1 :: Num a => a -> a -> a 
addV1 x y = x + y

addV2 :: Num a => a -> a -> a
addV2 x = (\y -> x + y)

add :: Num a => a -> a -> a
add = (\x -> (\y -> x + y))

mult :: Num a => a -> a -> a
mult x y = x * y

multV2 :: Num a => a -> a -> a
multV2 = (\x -> (\y -> x * y))

multBySum :: (Num a, Num b, Ord b) => a -> b -> a
multBySum num count
  | count <= 0 = 0
  | count == 1 = num
  | otherwise = num * multBySum num (count - 1)
  

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

  let mult10 = multBySum 1.1
  print $ mult10 10
  