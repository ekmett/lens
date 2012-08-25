{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.HashMap.Lens
  ( at
  , traverseMap
  , traverseAt
  ) where

import Control.Applicative as Applicative
import Control.Lens.Traversal
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.IndexedTraversal
import Data.Hashable
import Data.HashMap.Lazy as HashMap

-- | This 'Lens' can be used to read, write or delete the value associated with a key in a 'HashMap'.
--
-- >>> :m + Control.Lens Data.HashMap.Lens
--
-- >>> HashMap.fromList [("hello",12)] ^.at "hello"
-- Just 12
--
-- >>> at 10 .~ Just "hello" $ HashMap.empty
-- fromList [(10,"hello")]
at :: (Eq k, Hashable k) => k -> SimpleIndexedLens k (HashMap k v) (Maybe v)
at k = index $ \f m -> (`go` m) <$> f k (HashMap.lookup k m) where
  go Nothing   = HashMap.delete k
  go (Just v') = HashMap.insert k v'
{-# INLINE at #-}

-- | Traversal of a 'HashMap' indexed by the key.
--
-- @'traverseMap' = 'index' 'traverseWithKey'@
traverseMap :: IndexedTraversal k (HashMap k v) (HashMap k v') v v'
traverseMap = index traverseWithKey
{-# INLINE traverseMap #-}

-- | Traverse the value at a given key in a 'HashMap'
traverseAt :: (Eq k, Hashable k) => k -> SimpleIndexedTraversal k (HashMap k v) v
traverseAt k = at k <. traverse
{-# INLINE traverseAt #-}
