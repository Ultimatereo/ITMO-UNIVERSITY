{-# LANGUAGE TupleSections #-}

module HW3.T2
  ( distOption,
    wrapOption,
    distPair,
    wrapPair,
    distQuad,
    wrapQuad,
    distAnnotated,
    wrapAnnotated,
    distExcept,
    wrapExcept,
    distPrioritised,
    wrapPrioritised,
    distStream,
    wrapStream,
    distList,
    wrapList,
    distFun,
    wrapFun,
  )
where

import HW3.T1

-- distF implementation for Option
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some x, Some y) = Some (x, y)

-- wrapF implementation for Option
wrapOption :: a -> Option a
wrapOption = Some

-- distF implementation for Pair
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 y1, P x2 y2) = P (x1, x2) (y1, y2)

-- wrapF implementation for Pair
wrapPair :: a -> Pair a
wrapPair x = P x x

-- distF implementation for Quad
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q w1 x1 y1 z1, Q w2 x2 y2 z2) = Q (w1, w2) (x1, x2) (y1, y2) (z1, z2)

-- wrapF implementation for Quad
wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

-- distF implementation for Annotated
distAnnotated :: (Semigroup e) => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

-- wrapF implementation for Annotated
wrapAnnotated :: (Monoid e) => a -> Annotated e a
wrapAnnotated x = x :# mempty

-- distF implementation for Except
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e1, _) = Error e1
distExcept (_, Error e2) = Error e2
distExcept (Success x, Success y) = Success (x, y)

-- wrapF implementation for Except
wrapExcept :: a -> Except e a
wrapExcept = Success

-- distF implementation for Prioritised
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High x, High y) = High (x, y)
distPrioritised (High x, Medium y) = High (x, y)
distPrioritised (High x, Low y) = High (x, y)
distPrioritised (Medium x, High y) = High (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (Medium x, Low y) = Medium (x, y)
distPrioritised (Low x, High y) = High (x, y)
distPrioritised (Low x, Medium y) = Medium (x, y)
distPrioritised (Low x, Low y) = Low (x, y)

-- wrapF implementation for Prioritised
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- distF implementation for Stream
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x1 :> xs1, x2 :> xs2) = (x1, x2) :> distStream (xs1, xs2)

-- wrapF implementation for Stream
wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

-- distF implementation for List
distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (x :. xs, ys) = concatList (mapList (x,) ys) (distList (xs, ys))

concatList :: List a -> List a -> List a
concatList Nil list = list
concatList (x :. xs) list = x :. concatList xs list

-- wrapF implementation for List
wrapList :: a -> List a
wrapList x = x :. Nil

-- distF implementation for Fun
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\i -> (f i, g i))

-- wrapF implementation for Fun
wrapFun :: a -> Fun i a
wrapFun x = F (const x)
