{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.LruCache.Internal.PriorityQueue where

-- import Control.DeepSeq (seq)
import Data.List (sortOn, splitAt)

newtype PQueue p v = PQueue [(p, v)]
  deriving (Eq, Show)

insert :: Eq v => p -> v -> PQueue p v -> PQueue p v
insert !p !v (PQueue qas) =
  let lst = (p, v):filter ((/= v) . snd) qas
  in PQueue (forceSpine lst)

take :: Ord p => Int -> PQueue p v -> ([v], PQueue p v)
take n (PQueue qas) = case splitAt n (sortOn fst qas) of
  (as, bs) -> (snd <$> as, PQueue bs)

empty :: PQueue p v
empty = PQueue []

toList :: Ord p => PQueue p v -> [(p, v)]
toList (PQueue qas) = sortOn fst qas

size :: PQueue p v -> Int
size (PQueue qas) = length qas

forceSpine :: [a] -> [a]
forceSpine as =
  let fc = foldr (const id) () as
  in fc `seq` as
