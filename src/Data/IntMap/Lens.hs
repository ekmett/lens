{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.IntMap.Lens
  ( at
  , traverseIntMap
  , traverseAt
  , traverseAtMin
  , traverseAtMax
  ) where

import Control.Applicative as Applicative
import Control.Lens
import Data.IntMap as IntMap
import Data.Traversable

-- | This 'Lens' can be used to read, write or delete the value associated with a key in an 'IntMap'.
--
-- >>> fromList [(1,"hello")] ^.at 1
-- Just "hello"
--
-- >>> at 1 .~ Just "hello" $ IntMap.empty
-- fromList [(1,"hello")]
--
-- > at :: Int -> (Maybe v -> f (Maybe v)) -> IntMap v -> f (IntMap v)
at :: Int -> Simple Lens (IntMap v) (Maybe v)
at k f m = go <$> f (IntMap.lookup k m) where
  go Nothing   = IntMap.delete k m
  go (Just v') = IntMap.insert k v' m
{-# INLINE at #-}

-- | Traversal of an 'IntMap' indexed by the key.
traverseIntMap :: IndexedTraversal Int (IntMap v) (IntMap v') v v'
traverseIntMap = index $ \f -> sequenceA . mapWithKey f
{-# INLINE traverseIntMap #-}

-- | Traverse the value at a given key in an IntMap
--
-- > traverseAt :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
-- > traverseAt k = at k . traverse
traverseAt :: Int -> Simple Traversal (IntMap v) v
traverseAt k = at k . traverse
{-# INLINE traverseAt #-}

-- | Traverse the value at the minimum key in a Map
--
-- The key of the minimum element is available as the index.
traverseAtMin :: SimpleIndexedTraversal Int (IntMap v) v
traverseAtMin = index $ \f m -> case IntMap.minViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
  Just ((k,a), _) -> (\v -> IntMap.updateMin (const (Just v)) m) <$> f k a
#else
  Just ((k,a), _) -> (\v -> IntMap.updateMin (const v) m) <$> f k a
#endif
  Nothing     -> pure m
{-# INLINE traverseAtMin #-}

-- | Traverse the value at the maximum key in a Map
traverseAtMax :: SimpleIndexedTraversal Int (IntMap v) v
traverseAtMax = index $ \f m -> case IntMap.maxViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> (\v -> IntMap.updateMax (const (Just v)) m) <$> f k a
#else
    Just ((k,a), _) -> (\v -> IntMap.updateMax (const v) m) <$> f k a
#endif
    Nothing     -> pure m
{-# INLINE traverseAtMax #-}
