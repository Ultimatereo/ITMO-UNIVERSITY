module HW2.T1
  ( Tree (..),
    tfoldr,
  )
where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

type Function a b = (a -> b -> b)

tfoldr :: Function a b -> b -> Tree a -> b
tfoldr _ acc Leaf = acc
tfoldr f acc (Branch _ l val r) =
  tfoldr f midAcc l
  where
    rightAcc = tfoldr f acc r
    midAcc = f val rightAcc
