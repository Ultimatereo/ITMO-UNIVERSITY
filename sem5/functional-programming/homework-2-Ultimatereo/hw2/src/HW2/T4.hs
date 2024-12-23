module HW2.T4
  ( DotString (..),
    Fun (..),
    Inclusive (..),
    ListPlus (..),
  )
where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show, Eq)

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last x <> ys = x :+ ys
  (x :+ xs) <> ys = x :+ (xs <> ys)

data Inclusive a b = This a | That b | Both a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This x <> This y = This (x <> y)
  This x <> That y = Both x y
  This x <> Both y z = Both (x <> y) z
  That x <> This y = Both y x
  That x <> That y = That (x <> y)
  That x <> Both y z = Both y (x <> z)
  Both x y <> This z = Both (x <> z) y
  Both x y <> That z = Both x (y <> z)
  Both x y <> Both z w = Both (x <> z) (y <> w)

newtype DotString = DS String
  deriving (Show, Eq)

instance Semigroup DotString where
  DS s1 <> DS s2 = DS $ case (s1, s2) of
    ("", _) -> s2
    (_, "") -> s1
    ([x], ys) -> x : '.' : ys
    (x : xs, ys) -> let DS sTail = DS xs <> DS ys in x : sTail

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
