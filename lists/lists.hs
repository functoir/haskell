module MyLists(
  MyLists.asc,
  MyLists.sum
) where
import Data.List

asc :: (Ord t, Num t) => t -> t -> [t]
asc lo hi
  | lo > hi = []
  | lo == hi = [hi]
  | otherwise = lo : asc (lo + 1) hi

sum :: (Foldable t, Num b) => t b -> b
sum = foldr (+) 0

main :: IO ()
main = do
  let myList = asc 3 10
  print myList
  putStr "is null: "
  print (null myList)
  putStr "sum : "
  print (MyLists.sum myList)
