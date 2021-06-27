{-|
  File: parallels.hs
  Purpose: Basic Parallel computing in Haskell.
  Author: siavava <amittaijoel@outlook.com>
-}

import Control.Parallel ( par, pseq )
import GHC.Types ( Any )

{-|
  main -> Driver
-}
main :: IO ()
main = a `par` b `par` c `pseq` print (a + b + c)
  where
    a = ack 3 10
    b = fac 42
    c = fib 34

{-|
  fac -> finds the n'th factorial
-}
fac :: (Eq p, Num p) => p -> p
fac 0 = 1
fac n = n * fac(n-1)

{-|
  The ack function takes in two numbers, MAGIC, produces a single number!
-}
ack :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ack 0 n = n + 1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

{-|
  fib -> finds the n'th fibonacci number
-}
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)