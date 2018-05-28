module Arbor.LruCache.Type where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map               as M

data CacheConfig = CacheConfig
  { _cacheConfigMaxRequestsInFlight :: Int
  , _cacheConfigMaxSize             :: Int
  } deriving (Eq, Show)

data Cache k v = Cache
  { _cacheConfig           :: CacheConfig
  , _cacheRequestsInFlight :: STM.TVar Int
  , _cacheEntries          :: STM.TVar (M.Map k (STM.TVar (Maybe v)))
  , _cacheRetrieve         :: k -> IO v
  , _cacheEvict            :: k -> v -> IO ()
  }
