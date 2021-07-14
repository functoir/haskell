module Main where

-- ! 01: reverse a list of

-- | function to reverse an array using foldr
rev1 :: [a] -> [a]
rev1 = foldr (\x acc -> acc ++ [x]) []


-- | function to reverse an array using foldl
rev2 :: [a] -> [a]
rev2 = foldl (\acc x -> x : acc) []

-- | function to reverse an array using foldl
rev3 :: [a] -> [a]
rev3 = foldl (flip (:)) []



-- ! 02: find all prefixes of a list

prefixes1 :: [a] -> [[a]]
prefixes1 = foldr (\x acc -> [x] : map (x :) acc) []

prefixes2 :: [a] -> [[a]]
prefixes2 = foldr (\x acc -> map (x :) acc ++ [[x]]) []

prefixes3 :: [a] -> [[a]]
prefixes3 [] = []
prefixes3 [x] = [[x]]
prefixes3 (x:y:xs) = [x] : map (x :) (prefixes3 (y:xs))


-- ! 03: generate lagrange polynomials

lagrange :: [(Float, Float)] -> Float -> Float 
lagrange xs x = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs
  where
    l xj = foldl calc 1 xs
      where
        calc acc (xk, _)
          | xj == xk = acc
          | otherwise = acc * (x - xk) / (xj - xk)

main :: IO ()
main = do
  let myArray = [1, 2, 3, 4, 5]
  print $ "My array: " ++ show myArray
  print $ "Using rev1: " ++ show (rev1 myArray)
  print $ "Using rev2: " ++ show (rev2 myArray)
  print $ "Using rev3: " ++ show (rev3 myArray)

  print $ "prefixes1: " ++ show (prefixes1 myArray)

  print $ "prefixes2: " ++ show (prefixes2 myArray)

  print $ "prefixes3: " ++ show (prefixes3 myArray)

  let myExp = lagrange [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)]
  
  print $ "1.0 : " ++ show (myExp 1.0)
  print $ "2.0 : " ++ show (myExp 2.0)
  print $ "3.0 : " ++ show (myExp 3.0)
  print $ "4.0 : " ++ show (myExp 4.0)
  print $ "5.0 : " ++ show (myExp 5.0)
  print $ "6.0 : " ++ show (myExp 6.0)
  print $ "7.0 : " ++ show (myExp 7.0)
  print $ "8.0 : " ++ show (myExp 8.0)
  print $ "9.0 : " ++ show (myExp 9.0)
  print $ "10.0 : " ++ show (myExp 10.0)
  print $ "11.0 : " ++ show (myExp 11.0)

