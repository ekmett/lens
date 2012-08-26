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
  ( traverseIntMap
  , traverseAtMin
  , traverseAtMax
  ) where

import Control.Applicative as Applicative
import Control.Lens
import Data.IntMap as IntMap
import Data.Traversable

-- | Traversal of an 'IntMap' indexed by the key.
traverseIntMap :: IndexedTraversal Int (IntMap v) (IntMap v') v v'
traverseIntMap = index $ \f -> sequenceA . mapWithKey f
{-# INLINE traverseIntMap #-}

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
