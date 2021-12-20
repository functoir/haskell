module Main where

newtype Picture = Picture {
  _data :: [String]
} deriving (Eq)

instance Show Picture where
  show (Picture _data) = unlines _data

flipH :: Picture -> Picture
flipH (Picture _data) = Picture (reverse _data)

flipV :: Picture -> Picture
flipV (Picture _data) = Picture (map reverse _data)

above :: Picture -> Picture -> Picture
below :: Picture -> Picture -> Picture
beside :: Picture -> Picture -> Picture
above (Picture data1) (Picture data2) = Picture (data1 ++ data2)
below (Picture data1) (Picture data2) = Picture (data2 ++ data1)
beside (Picture data1) (Picture data2) = Picture (zipWith (++) data1 data2)



main :: IO ()
main = do
  let pic = Picture ["abcd", "efgh", "ijkl", "mnop"]
  let pic2 = flipH pic
  let pic3 = flipV pic
  print pic
  print pic2
  print pic3
  print $ above pic pic2
  print $ below pic pic2
  print $ beside pic pic3
