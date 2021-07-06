import GHC.Types ( Any )

-- | File: folding.hs
--   Purpose: basic folding in Haskell
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/06/2021



-- sum :: Any Integer -> Integer
-- sum = foldr (+) 0 


max :: (Num p, Ord p) => [p] -> p
max [] = 0
max (x:xs)
  | x > Main.max xs = x
  | otherwise = Main.max xs

main :: IO ()
main =
  print (Main.max [1, 2, 3, 19, 4, 5, 6])
