module HW0.T4
  ( fac,
    fib,
    map',
    repeat',
  )
where

import Data.Function (fix)
import Numeric.Natural (Natural)

-- repeat' function using fix
repeat' :: a -> [a]
repeat' = fix . (:)

-- map' function using fix
map' :: (a -> b) -> [a] -> [b]
map' f =
  fix
    ( \rec xs -> case xs of
        (y : ys) -> f y : rec ys
        [] -> []
    )

-- fib function using fix
fib :: Natural -> Natural
fib x = fibGet (x, 1, 0)
  where
    fibGet = fix (\rec (n, f1, f2) -> if n == 0 then f2 else rec (n - 1, f1 + f2, f1))

-- fac function using fix
fac :: Natural -> Natural
fac = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))
