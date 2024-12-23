module HW2.T2
  ( joinWith,
    splitOn,
  )
where

import Data.List.NonEmpty (NonEmpty (..), (<|))

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn sep xs = case break (== sep) xs of
  (word, []) -> word :| []
  (word, _ : cons) -> word <| splitOn sep cons

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ ([] :| []) = []
joinWith sep ([] :| (x : xs)) = sep : joinWith sep (x :| xs)
joinWith sep ((x : xs) :| contTail) = x : joinWith sep (xs :| contTail)
