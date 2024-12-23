module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,
    ncmp,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Numeric.Natural

data N = Z | S N deriving (Show)

nplus :: N -> N -> N
nplus Z n = n
nplus (S cons) n = nplus cons (S n)

nmult :: N -> N -> N
nmult Z n = Z
nmult (S Z) n = n
nmult (S cons) n = nplus n (nmult cons n)

nsub :: N -> N -> Maybe N
nsub Z Z = Just Z
nsub Z n = Nothing
nsub (S n) Z = Just (S n)
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S n) (S m) = ncmp n m

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: (Num a) => N -> a
nToNum Z = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z = True
nEven (S n) = not (nEven n)

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv n m = case nsub n m of
  Nothing -> Z
  Just res -> S (ndiv res m)

nmod :: N -> N -> N
nmod n m = case nsub n m of
  Nothing -> n
  Just res -> nmod res m