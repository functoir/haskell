

isZeroPattern :: (Eq a, Num a) => a -> Bool
isZeroPattern 0 = True
isZeroPattern _ = False

isZeroGuarded :: (Eq a, Num a) => a -> Bool
isZeroGuarded x
  | x == 0 = True
  | otherwise = False

-- | factorials
fac :: (Ord t, Num t) => t -> t
fac n = aux n 1
  where
    aux n acc                               -- use auxilliary function
      | n <= 1 = acc                        -- factorial 0, 1 = 1
      | otherwise = aux (n-1) (n * acc)     -- factorial n = n * factorial n-1

sum :: (Foldable t, Num b) => t b -> b
sum = foldr (+) 0

main :: IO ()
main =
  print (fac 5)
