# arbor-lru-cache

A thread-safe LRU cache library.

## Example

To use the cache:

```haskell
main :: IO
main = do
  -- Provide a configuration that includes how many simultaneous in
  -- flight requests are allowed and now many entries the cache can store
  let config = A.CacheConfig
        { A.maxRequestsInFlight = 1
        , A.maxOccupancy        = 1
        }

  -- Create a cache providing the config and functions that handle retrieval
  -- and eviction.
  cache <- A.makeCache config retrieve evict

  -- Perform your lookups
  _ <- A.lookup 1 cache
  _ <- A.lookup 2 cache
  _ <- A.lookup 3 cache

  return ()

-- Implement value retrieval function.  If the same key is looked up multiple
-- times, the cache guarantees that the retrieve function is called only once
-- for that key up until it is evicted.
retrieve :: Int -> IO String
retrieve mk = ...

-- Perform any cleanup that should occur when an entry is evicted
-- Please be aware that if your code is concurrent, the eviction function may
-- be called whilst the you code is concurrently using a value it has looked up.
-- Your code is wholly responsible for ensuring this case still works.
evict :: Int -> String -> IO ()
evict mk mv = ...
```
