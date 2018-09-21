module Arbor.LruCache.LruCacheSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Arbor.LruCache as A

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

data Event
  = EvictEvent
  { key   :: Int
  , value :: String
  }
  | RetrieveEvent
  { key   :: Int
  , value :: String
  } deriving (Eq, Show)

spec :: Spec
spec = describe "Arbor.LruCache.LruCacheSpec" $ do
  it "Insertion dedupes by value" $ requireTest $ do
    th <- liftIO $ newTVarIO []
    let config = A.CacheConfig
          { A.maxRequestsInFlight = 1
          , A.maxOccupancy        = 1
          }
    cache <- liftIO $ A.makeCache config (retrieve th) (evict th)

    _ <- liftIO $ A.lookup 1 cache
    _ <- liftIO $ A.lookup 2 cache
    _ <- liftIO $ A.lookup 3 cache

    history <- liftIO $ atomically $ reverse <$> readTVar th
    history ===
      [ RetrieveEvent { key = 1 , value = "1" }
      , RetrieveEvent { key = 2 , value = "2" }
      , EvictEvent    { key = 1 , value = "1" }
      , RetrieveEvent { key = 3 , value = "3" }
      , EvictEvent    { key = 2 , value = "2" }
      ]

retrieve :: TVar [Event] -> Int -> IO String
retrieve th mk = do
  let mv = show mk
  atomically $ modifyTVar th (RetrieveEvent mk mv:)
  return mv

evict :: TVar [Event] -> Int -> String -> IO ()
evict th mk mv = atomically $ modifyTVar th (EvictEvent mk mv:)
