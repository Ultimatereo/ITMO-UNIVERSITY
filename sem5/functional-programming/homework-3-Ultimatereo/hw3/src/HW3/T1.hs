module HW3.T1
  ( Option (..),
    Pair (..),
    Quad (..),
    Annotated (..),
    Except (..),
    Prioritised (..),
    Stream (..),
    List (..),
    Fun (..),
    Tree (..),
    mapOption,
    mapPair,
    mapQuad,
    mapAnnotated,
    mapExcept,
    mapPrioritised,
    mapStream,
    mapList,
    mapFun,
    mapTree,
  )
where

data Option a = None | Some a
  deriving (Show, Eq)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None = None
mapOption f (Some x) = Some (f x)

data Pair a = P a a
  deriving (Show, Eq)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P x y) = P (f x) (f y)

data Quad a = Q a a a a
  deriving (Show, Eq)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q w x y z) = Q (f w) (f x) (f y) (f z)

data Annotated e a = a :# e
  deriving (Show, Eq)

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (x :# e) = f x :# e

data Except e a = Error e | Success a
  deriving (Show, Eq)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept f (Success x) = Success (f x)

data Prioritised a = Low a | Medium a | High a
  deriving (Show, Eq)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low x) = Low (f x)
mapPrioritised f (Medium y) = Medium (f y)
mapPrioritised f (High z) = High (f z)

data Stream a = a :> Stream a
  deriving (Show, Eq)

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> xs) = f x :> mapStream f xs

data List a = Nil | a :. List a
  deriving (Show, Eq)

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList f (x :. xs) = f x :. mapList f xs

newtype Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (f . g)

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Show, Eq)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch left x right) = Branch (mapTree f left) (f x) (mapTree f right)
