{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Arbor.LruCache.Internal.Lens where

import Arbor.LruCache.Type
import Control.Lens

makeFields ''Cache
makeFields ''CacheConfig
