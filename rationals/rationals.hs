{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Exception.Base (Exception)
import Unsafe.Coerce ()

data RAT =
  {- | RAT data type for rational numbers.
    NOTE: To initialize a rat, use the `initRAT` function,
    which initializes and refactors it as opposed to the `RAT`
    constructor which stores unfactored values.
  -}
  RAT {
    num :: Int, -- ^ Numerator
    denom :: Int -- ^ Denominator
  }

-- ! Initializer

-- | Create and initialize a rational number.
initRAT :: Int -> Int -> RAT
initRAT num denom = normRAT (RAT num denom)

-- ! Instances of RAT

instance Show RAT where
  show RAT {num, denom} = show num ++ "/" ++ show denom

instance Eq RAT where
  (==) :: RAT -> RAT -> Bool
  (==) p q = num p * denom q == num q * denom p

instance Ord RAT where
  compare :: RAT -> RAT -> Ordering
  compare p q = compare (num p * denom q)  (num q * denom p)


-- ! HELPERS

-- | The `signum` function finds the sign of a number.
signum' ::  Integral a => a -> a
signum' x = if x < 0 then -1 else 1

-- | The `abs'` function finds the absolute value of a number.
abs' :: (Ord p, Num p) => p -> p
abs' x = if x < 0 then -x else x

-- | The `gcd` function finds the greatest common divisor of two numbers.
gcd' :: Integral t => t -> t -> t
gcd' x y = gcd'' (abs' x) (abs' y)
  where
    gcd'' x 0 = x
    gcd'' x y = gcd'' y (x `rem` y)


-- ! RATIONAL NUMBER OPERATIONS


errRAT :: RAT -> String 
errRAT r = error "Invalid rational number: " ++ show r

-- | the `normRAT` function normalizes a rational number.
normRAT :: RAT -> RAT
normRAT r
  | denom r == 0 = error ("Invalid rational number: " ++ show r)
  | num r == 0 = RAT 0 1
  | otherwise = RAT (a `div` d) (b `div` d)
  where
    a = signum' (denom r) * num r       -- Find sign of denominator, convert numerator
    b = abs' (denom r)                  -- Find absolute value of denominator
    d = gcd' a b                        -- Find greatest common divisor of numerator and denominator


-- | The `negRAT` function negates a rational number.
negRAT :: RAT -> RAT
negRAT RAT {num, denom} = initRAT (-num) denom

-- | The `recRAT` function finds the reciprocal of a rational number.
recRAT :: RAT -> RAT
recRAT RAT {num, denom} = initRAT denom num


-- ! PRIMITIVE OPERATIONS

(!+), (!-), (!*), (!/) :: RAT -> RAT -> RAT

-- | Add two rational numbers.
(!+) p q = initRAT (num p*denom q + num q*denom p) (denom p * denom q)

-- | Subtract two rational numbers.
(!-) p q = initRAT (num p*denom q - num q * denom p) (denom p * denom q)

-- | Multiply two rational numbers.
(!*) p q = initRAT (num p*num q) (denom p * denom q)

-- | Divide two rational numbers.
(!/) p q = initRAT (num p*denom q) (num q * denom p)


main :: IO ()
main = do

  let someRAT = initRAT 12 32
  let otherRAT = initRAT 24 32
  let thirdRAT = initRAT 16 32

  putStr "\n\n"
  print $ "someRAT = " ++ show someRAT
  print $ "otherRAT = " ++ show otherRAT
  print $ "thirdRAT = " ++ show thirdRAT
  putStr "\n\n"
  print $ "someRAT == otherRAT " ++ show (someRAT == otherRAT)
  print $ "someRAT == thirdRAT " ++ show (someRAT == thirdRAT)
  print $ "someRAT < otherRAT " ++ show (someRAT < otherRAT)
  print $ "someRAT < thirdRAT " ++ show (someRAT < thirdRAT)
  print $ "someRAT > thirdRAT " ++ show (someRAT > thirdRAT)
  putStr "\n\n"
  print $ show someRAT ++ " + " ++ show otherRAT ++ " = " ++ show (someRAT !+ otherRAT)
  print $ show someRAT ++ " - " ++ show otherRAT ++ " = " ++ show (someRAT !- otherRAT)
  print $ show someRAT ++ " x " ++ show otherRAT ++ " = " ++ show (someRAT !* otherRAT)
  print $ show someRAT ++ " / " ++ show otherRAT ++ " = " ++ show (someRAT !/ otherRAT)
  putStr "\n\n"
  print $ show someRAT ++ " + " ++ show thirdRAT ++ " = " ++ show (someRAT !+ thirdRAT)
  print $ show someRAT ++ " - " ++ show thirdRAT ++ " = " ++ show (someRAT !- thirdRAT)
  print $ show someRAT ++ " x " ++ show thirdRAT ++ " = " ++ show (someRAT !* thirdRAT)
  print $ show someRAT ++ " / " ++ show thirdRAT ++ " = " ++ show (someRAT !/ thirdRAT)
  putStr "\n\n"
  print $ show someRAT ++ " + " ++ show otherRAT ++ " + " ++ show thirdRAT ++ " = " ++ show (someRAT !+ otherRAT !+ thirdRAT)


  print (RAT 3 0)
  -- print (initRAT 3 0) 
