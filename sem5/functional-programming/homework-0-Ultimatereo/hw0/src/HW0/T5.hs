module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

-- Nat type
type Nat a = (a -> a) -> a -> a

-- Zero constructor
nz :: Nat a
nz _ z = z

-- Successor constructor
ns :: Nat a -> Nat a
ns n f z = f (n f z)

-- Addition operation
nplus :: Nat a -> Nat a -> Nat a
nplus m n f z = n f (m f z)

-- Multiplication operation
nmult :: Nat a -> Nat a -> Nat a
nmult m n f = m (n f)

-- Convert a Natural number to a Church numeral
nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1

-- Convert a Church numeral to a standard number
nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0
