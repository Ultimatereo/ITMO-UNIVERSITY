module HW6.T1
  ( BucketsArray
  , CHT (..)
  
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM)
import Control.Concurrent.Classy.STM (TArray, TVar)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: m (CHT (STM m) k v)
-- newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = undefined

-- getCHT
--   :: ( MonadConc m
--      , Eq k
--      , Hashable k
--      )
--   => k
--   -> CHT (STM m) k v
--   -> m (Maybe v)
getCHT :: k -> CHT (STM m) k v -> m (Maybe v)
getCHT = undefined

-- putCHT
--   :: ( MonadConc m
--      , Eq k
--      , Hashable k
--      )
--   => k
--   -> v
--   -> CHT (STM m) k v
--   -> m ()
putCHT :: k -> v -> CHT (STM m) k v -> m ()
putCHT = undefined

-- sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT :: CHT (STM m) k v -> m Int
sizeCHT = undefined
