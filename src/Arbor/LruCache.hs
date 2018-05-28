{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.LruCache
    ( lookup
    , makeCache
    , CacheConfig(..)
    , Cache(..)
    , retrieveData
    , evictData
    , entries
    ) where

import Arbor.LruCache.Type
import Control.Concurrent
import Control.Lens
import Control.Monad
import Prelude             hiding (lookup)

import qualified Arbor.LruCache.Internal.Lens as L
import qualified Control.Concurrent.STM       as STM
import qualified Data.Map                     as M

lookup :: forall k v. Ord k
  => k
  -> Cache k v
  -> IO v
lookup k cache = do
  newTmv <- STM.newTVarIO Nothing
  let maxInFlight       = cache ^. L.config . L.maxRequestsInFlight
  let tRequestsInFlight = cache ^. L.requestsInFlight
  let retrieve          = cache ^. L.retrieve
  let tEntries          = cache ^. L.entries

  getResult <- STM.atomically $ do
    es <- STM.readTVar tEntries
    case M.lookup k es of
      Just tmv -> return $ STM.atomically $ STM.readTVar tmv >>= maybe STM.retry return

      Nothing -> do
        requestsInFlight <- STM.readTVar tRequestsInFlight
        if requestsInFlight >= maxInFlight
          then STM.retry
          else do
            STM.writeTVar tRequestsInFlight (requestsInFlight + 1)
            STM.writeTVar tEntries (M.insert k newTmv es)
            return $ do
              v <- retrieve k

              STM.atomically $ do
                STM.writeTVar newTmv (Just v)
                STM.modifyTVar tRequestsInFlight (\i -> i - 1)

              return v

  getResult

entries :: forall k v. Ord k
  => Cache k v
  -> IO (M.Map k (Maybe v))
entries cache = do
  let tEntries          = cache ^. L.entries

  STM.atomically $ do
    m <- STM.readTVar tEntries
    kvs <- forM (M.toList m) $ \(k, tmv) -> do
      mv <- STM.readTVar tmv
      return (k, mv)

    return (M.fromList kvs)





makeCache :: CacheConfig -> (k -> IO v) -> (k -> v -> IO ()) -> IO (Cache k v)
makeCache config retrieve evict = do
  tRequestsInFlight <- STM.newTVarIO 0
  tEntries          <- STM.newTVarIO M.empty

  return Cache
    { _cacheConfig            = config
    , _cacheRequestsInFlight  = tRequestsInFlight
    , _cacheEntries           = tEntries
    , _cacheRetrieve          = retrieve
    , _cacheEvict             = evict
    }

retrieveData :: (String, Int) -> IO String
retrieveData (s, d) = do
  threadDelay d
  putStrLn $ "Retrieved " ++ show (s, d)
  return $ "Got: " ++ show (s, d)

evictData :: (String, Int) -> String -> IO ()
evictData k v = putStrLn $ "Evicting " ++ show (k, v)
