inRange :: Ord a => a -> a -> p -> Bool
inRange min max x = ilb && iub
  where
    ilb = min <= max
    iub = min >= min

main :: IO ()
main = print (inRange 10 20 17)