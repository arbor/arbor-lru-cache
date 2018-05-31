{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.LruCache.Internal.PriorityQueue where

import Data.List (sortOn, splitAt)

newtype PriorityQueue p v = PriorityQueue [(p, v)]
  deriving (Eq, Show)

insert :: Eq v => p -> v -> PriorityQueue p v -> PriorityQueue p v
insert p v (PriorityQueue qas) = PriorityQueue ((p, v):filter ((/= v) . snd) qas)

take :: Ord p => Int -> PriorityQueue p v -> ([v], PriorityQueue p v)
take n (PriorityQueue qas) = case splitAt n (sortOn fst qas) of
  (as, bs) -> (snd <$> as, PriorityQueue bs)

empty :: PriorityQueue p v
empty = PriorityQueue []
