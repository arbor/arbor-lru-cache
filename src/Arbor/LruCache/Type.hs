{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arbor.LruCache.Type where

import GHC.Generics

import qualified Arbor.LruCache.Internal.PriorityQueue as PQ
import qualified Control.Concurrent.STM                as STM
import qualified Data.Map                              as M

data CacheConfig = CacheConfig
  { maxRequestsInFlight :: Int
  , maxOccupancy        :: Int
  } deriving (Eq, Show, Generic)

data Cache k v = Cache
  { config           :: CacheConfig
  , requestsInFlight :: STM.TVar Int
  , entries          :: STM.TVar (M.Map k (STM.TVar (Maybe v)))
  , evictionQueue    :: STM.TVar (PQ.PriorityQueue Int k)
  , evictionPriority :: STM.TVar Int
  , occupancy        :: STM.TVar Int
  , retrieve         :: k -> IO v
  , evict            :: k -> v -> IO ()
  } deriving Generic
