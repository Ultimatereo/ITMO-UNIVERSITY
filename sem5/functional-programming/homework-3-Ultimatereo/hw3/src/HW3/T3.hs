module HW3.T3
  ( joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW3.T1

-- joinF implementation for Option
joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some innerOption) = innerOption

-- joinF implementation for Except
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success innerExcept) = innerExcept

-- joinF implementation for Annotated
joinAnnotated :: (Semigroup e) => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# e2 <> e1

-- joinF implementation for List
joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. xs) = joinList xs
joinList ((x :. xs) :. xTail) = x :. joinList (xs :. xTail)

-- joinF implementation for Fun
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> let (F g) = f x in g x)
