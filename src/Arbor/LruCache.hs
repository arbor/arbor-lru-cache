{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Arbor.LruCache
    ( lookup
    , makeCache
    , Z.CacheConfig(..)
    , Z.Cache(..)
    , retrieveData
    , evictData
    , entries
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function      ((&))
import Data.Maybe
import Prelude            hiding (lookup)

-- import qualified Arbor.LruCache.Internal.PriorityQueue as PQ
import qualified Arbor.LruCache.Internal.PriorityQueue as PQ
import qualified Arbor.LruCache.Type                   as Z
import qualified Control.Concurrent.STM                as STM
import qualified Data.Map.Strict                       as M

lookup :: Ord k => k -> Z.Cache k v -> IO v
lookup k cache = do
  newTmv <- STM.newTVarIO Nothing
  let maxInFlight       = cache & Z.config & Z.maxRequestsInFlight
  let evict             = cache & Z.evict
  let tRequestsInFlight = cache & Z.requestsInFlight
  let retrieve          = cache & Z.retrieve
  let tEntries          = cache & Z.entries
  let tOccupancy        = cache & Z.occupancy

  join $ STM.atomically $ do
    es <- STM.readTVar tEntries
    case M.lookup k es of
      Just tmv -> do
        registerForEviction k cache
        return $ STM.atomically $ STM.readTVar tmv >>= maybe STM.retry return

      Nothing -> do
        requestsInFlight <- STM.readTVar tRequestsInFlight
        if requestsInFlight >= maxInFlight
          then STM.retry
          else do
            STM.writeTVar tRequestsInFlight (requestsInFlight + 1)
            STM.writeTVar tEntries (M.insert k newTmv es)
            return $ do
              v <- catch (retrieve k) $ \(e :: SomeException) -> do
                STM.atomically $ do
                  entries2 <- STM.readTVar tEntries
                  forM_ (M.lookup k entries2) $ \tv -> STM.writeTVar tv (throw e)
                  STM.modifyTVar' tRequestsInFlight pred
                  STM.writeTVar tEntries (M.delete k entries2)
                throw e

              kvsForEviction <- STM.atomically $ do
                STM.writeTVar newTmv (Just v)
                STM.modifyTVar' tRequestsInFlight pred
                STM.modifyTVar' tOccupancy succ

                registerForEviction k cache
                takeEvictionsDue cache

              forM_ kvsForEviction $ uncurry evict

              return v

registerForEviction :: Eq k => k -> Z.Cache k v -> STM.STM ()
registerForEviction k cache = do
  let tEvictionQueue    = cache & Z.evictionQueue
  let tEvictionPriority = cache & Z.evictionPriority

  STM.modifyTVar' tEvictionPriority (+1)
  evictionPriority <- STM.readTVar tEvictionPriority
  STM.modifyTVar' tEvictionQueue (PQ.insert evictionPriority k)

takeEvictionsDue :: Ord k => Z.Cache k v -> STM.STM [(k, v)]
takeEvictionsDue cache = do
  let maxOccupancy      = cache & Z.config & Z.maxOccupancy
  let tEntries          = cache & Z.entries
  let tOccupancy        = cache & Z.occupancy
  let tEvictionQueue    = cache & Z.evictionQueue

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

  let kvs = catMaybes mkvs

  STM.writeTVar tEntries (foldl (flip M.delete) es (fst <$> kvs))

  return kvs

entries :: Ord k => Z.Cache k v -> IO (M.Map k (Maybe v))
entries cache = do
  let tEntries          = cache & Z.entries

  STM.atomically $ do
    m <- STM.readTVar tEntries
    kvs <- forM (M.toList m) $ \(k, tmv) -> do
      mv <- STM.readTVar tmv
      return (k, mv)

    return (M.fromList kvs)

makeCache :: Z.CacheConfig -> (k -> IO v) -> (k -> v -> IO ()) -> IO (Z.Cache k v)
makeCache config retrieve evict = do
  tRequestsInFlight <- STM.newTVarIO 0
  tEntries          <- STM.newTVarIO M.empty
  tOccupancy        <- STM.newTVarIO 0
  tEvictionQueue    <- STM.newTVarIO PQ.empty
  tEvictionPriority <- STM.newTVarIO 0

  return Z.Cache
    { Z.config            = config
    , Z.requestsInFlight  = tRequestsInFlight
    , Z.entries           = tEntries
    , Z.evictionQueue     = tEvictionQueue
    , Z.evictionPriority  = tEvictionPriority
    , Z.occupancy         = tOccupancy
    , Z.retrieve          = retrieve
    , Z.evict             = evict
    }

retrieveData :: (String, Int) -> IO String
retrieveData (s, d) = do
  threadDelay d
  putStrLn $ "Retrieved " ++ show (s, d)
  return $ "Got: " ++ show (s, d)

evictData :: (String, Int) -> String -> IO ()
evictData k v = putStrLn $ "Evicting " ++ show (k, v)
