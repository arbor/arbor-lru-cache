module Arbor.LruCache.Internal.PriorityQueueSpec (spec) where

import Data.Function
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Arbor.LruCache.Internal.PriorityQueue as PQ

spec :: Spec
spec = describe "Arbor.LruCache.Internal.PriorityQueueSpec" $ do
  it "Insertion dedupes by value" $ requireTest $ do
    let actual = (PQ.empty :: PQ.PQueue Int Int)
          & PQ.insert 1 101
          & PQ.insert 3 101
          & PQ.insert 2 101

    let expected = [(2, 101)]

    PQ.toList actual === expected
  it "Insert stores all non-duplicate values" $ requireTest $ do
    let actual = (PQ.empty :: PQ.PQueue Int Int)
          & PQ.insert 1 101
          & PQ.insert 3 303
          & PQ.insert 2 202

    let expected = [(1, 101), (2, 202), (3, 303)]

    PQ.toList actual === expected
  it "Take removes most priority element from queue" $ requireTest $ do
    let actual = (PQ.empty :: PQ.PQueue Int Int)
          & PQ.insert 1 101
          & PQ.insert 3 303
          & PQ.insert 2 202
          & PQ.take 1 & snd

    let expected = [(2, 202), (3, 303)]

    PQ.toList actual === expected
  it "Take yields most priority element from queue" $ requireTest $ do
    let actual = (PQ.empty :: PQ.PQueue Int Int)
          & PQ.insert 1 101
          & PQ.insert 3 303
          & PQ.insert 2 202
          & PQ.take 1 & fst

    let expected = [101]

    actual === expected


