module Arbor.LruCache.Type where

import qualified Arbor.LruCache.Internal.PriorityQueue as PQ
import qualified Control.Concurrent.STM                as STM
import qualified Data.Map                              as M

data CacheConfig = CacheConfig
  { _cacheConfigMaxRequestsInFlight :: Int
  , _cacheConfigMaxOccupancy        :: Int
  } deriving (Eq, Show)

data Cache k v = Cache
  { _cacheConfig           :: CacheConfig
  , _cacheRequestsInFlight :: STM.TVar Int
  , _cacheEntries          :: STM.TVar (M.Map k (STM.TVar (Maybe v)))
  , _cacheEvictionQueue    :: STM.TVar (PQ.PriorityQueue Int k)
  , _cacheEvictionPriority :: STM.TVar Int
  , _cacheOccupancy        :: STM.TVar Int
  , _cacheRetrieve         :: k -> IO v
  , _cacheEvict            :: k -> v -> IO ()
  }
