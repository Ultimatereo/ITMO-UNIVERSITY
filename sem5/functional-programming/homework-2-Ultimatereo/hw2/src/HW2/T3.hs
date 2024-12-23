{-# LANGUAGE LambdaCase #-}
module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable()

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap (\case {Just x -> x; Nothing -> mempty})

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap (\case {Left x -> (x, mempty); Right x -> (mempty, x)})
