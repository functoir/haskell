module Main where

import Data.Maybe (isJust, isNothing, fromJust)

{- >>= (bind) -}
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) = 

maybeadd :: Num b => Maybe b -> Maybe b -> Maybe b
maybeadd mx my = mx >>= (\x -> my >>= (\y -> return $ x+y))

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my = do
  x <- mx
  y <- my
  return $ x + y

-- hasValue :: (Monad m, Num b) => m b -> m Int
-- hasValue mx = mx >> const 1



main :: IO ()
main = do
  print $ show (maybeadd Nothing Nothing)
  print $ show (maybeadd Nothing (Just 1))
  print $ show (maybeadd (Just 5) Nothing)
  print $ show (maybeadd (Just 5) (Just 6))
  let my = maybeadd (Just 5) (Just 6)
  let y = fromJust my
  print $ show y
