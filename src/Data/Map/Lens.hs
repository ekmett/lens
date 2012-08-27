{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Map.Lens
  ( traverseAtMin
  , traverseAtMax
  ) where

import Control.Applicative as Applicative
import Control.Lens.Indexed
import Control.Lens.IndexedTraversal
import Data.Map as Map

-- | Traverse the value at the minimum key in a Map.
--
-- The key of the minimum element is available as the index of the traversal.
traverseAtMin :: SimpleIndexedTraversal k (Map k v) v
traverseAtMin = index $ \f m -> case Map.minViewWithKey m of
  Just ((k, a), _) -> (\v -> Map.updateMin (const (Just v)) m) <$> f k a
  Nothing          -> pure m
{-# INLINE traverseAtMin #-}

-- | Traverse the value at the maximum key in a Map.
--
-- The key of the maximum element is available as the index of the traversal.
traverseAtMax :: SimpleIndexedTraversal k (Map k v) v
traverseAtMax = index $ \f m -> case Map.maxViewWithKey m of
    Just ((k, a), _) -> (\v -> Map.updateMax (const (Just v)) m) <$> f k a
    Nothing          -> pure m
{-# INLINE traverseAtMax #-}
