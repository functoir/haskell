module Main where
import Control.Monad (void)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  void loop2
  putStrLn "Goodbye, Haskell!"

-- loop :: Functor a => a
loop :: (t0 -> t) -> t
loop = (\x -> x x) (\x -> x x)

loop2 :: t
loop2 = do
  loop2

recur f = (\x -> f (x x)) (\x -> f (x x))