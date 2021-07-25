module Main where

natToBin :: Int -> String
natToBin 0 = "0"
natToBin n = convert n ""
  where
    convert n word
      | n == 0 = word
      | n `rem` 2 == 1 = convert (n `div` 2) (word ++ "1")
      | otherwise = convert (n `div` 2) (word ++ "0")


main :: IO ()
main = do
  print $ "5 to binary = " ++ natToBin 5
  print $ "8 to binary = " ++ natToBin 8
  print $ "9 to binary = " ++ natToBin 9