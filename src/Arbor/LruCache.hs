{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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
import Data.Maybe
import Prelude             hiding (lookup)

import qualified Arbor.LruCache.Internal.Lens           as L
import qualified Control.Concurrent.STM                 as STM
import qualified Data.Map                               as M
import qualified HaskellWorks.Data.PriorityQueue.Strict as PQ

lookup :: forall k v. Ord k
  => k
  -> Cache k v
  -> IO v
lookup k cache = do
  newTmv <- STM.newTVarIO Nothing
  let maxInFlight       = cache ^. L.config . L.maxRequestsInFlight
  let evict             = cache ^. L.evict
  let tRequestsInFlight = cache ^. L.requestsInFlight
  let retrieve          = cache ^. L.retrieve
  let tEntries          = cache ^. L.entries
  let tOccupancy        = cache ^. L.occupancy

  join $ STM.atomically $ do
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

              kvsForEviction <- STM.atomically $ do
                STM.writeTVar newTmv (Just v)
                STM.modifyTVar tRequestsInFlight pred
                STM.modifyTVar tOccupancy succ

                registerForEviction k cache
                takeEvictionsDue cache


              forM_ kvsForEviction $ uncurry evict

              return v

registerForEviction :: k -> Cache k v -> STM.STM ()
registerForEviction k cache = do
  let tEvictionQueue    = cache ^. L.evictionQueue
  let tEvictionPriority = cache ^. L.evictionPriority

  STM.modifyTVar tEvictionPriority (+1)
  evictionPriority <- STM.readTVar tEvictionPriority
  STM.modifyTVar tEvictionQueue (PQ.insert evictionPriority k)

takeEvictionsDue :: Ord k => Cache k v -> STM.STM [(k, v)]
takeEvictionsDue cache = do
  let maxOccupancy      = cache ^. L.config . L.maxOccupancy
  let tEntries          = cache ^. L.entries
  let tOccupancy        = cache ^. L.occupancy
  let tEvictionQueue    = cache ^. L.evictionQueue

  evictionQueue <- STM.readTVar tEvictionQueue
  occupancy <- STM.readTVar tOccupancy

  if occupancy > maxOccupancy
    then case PQ.take (occupancy - maxOccupancy) evictionQueue of
      (ks, evictionQueue') -> do
        STM.writeTVar tEvictionQueue evictionQueue'
        STM.writeTVar tOccupancy maxOccupancy

        removeEvictionsFromEntries ks tEntries

    else return []

removeEvictionsFromEntries :: Ord k => [k] -> STM.TVar (M.Map k (STM.TVar (Maybe v))) -> STM.STM [(k, v)]
removeEvictionsFromEntries ks tEntries = do
  es <- STM.readTVar tEntries

  let kmtmvs = (\k -> (k, M.lookup k es)) <$> ks

  mkvs <- forM kmtmvs $ \(k, mtmv) -> case mtmv of
    Just tmv -> do
      mv <- STM.readTVar tmv
      return ((k, ) <$> mv)
    Nothing -> return Nothing

  return $ catMaybes mkvs

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
  tOccupancy        <- STM.newTVarIO 0
  tEvictionQueue    <- STM.newTVarIO PQ.empty
  tEvictionPriority <- STM.newTVarIO 0

  return Cache
    { _cacheConfig            = config
    , _cacheRequestsInFlight  = tRequestsInFlight
    , _cacheEntries           = tEntries
    , _cacheEvictionQueue     = tEvictionQueue
    , _cacheEvictionPriority  = tEvictionPriority
    , _cacheOccupancy         = tOccupancy
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
