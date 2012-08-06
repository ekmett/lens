{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( at
  , traverseMap
  , traverseAt
  , traverseAtMin
  , traverseAtMax
  ) where

import Control.Applicative as Applicative
import Control.Lens.Type
import Control.Lens.Traversal
import Control.Lens.Indexed
import Control.Lens.Getter -- used by tests
import Control.Lens.Setter -- used by tests
import Data.Map as Map
import Data.Traversable

-- | This 'Lens' can be used to read, write or delete the value associated with a key in a 'Map'.
--
-- >>> Map.fromList [("hello",12)] ^.at "hello"
-- Just 12
--
-- >>> at 10 .~ Just "hello" $ Map.empty
-- fromList [(10,"hello")]
--
-- > at :: Ord k => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
at :: Ord k => k -> SimpleLens (Map k v) (Maybe v)
at k f m = go <$> f (Map.lookup k m) where
  go Nothing   = Map.delete k m
  go (Just v') = Map.insert k v' m
{-# INLINE at #-}

-- | Traversal of a 'Map' indexed by the key.
traverseMap :: IndexedTraversal k (Map k v) (Map k v') v v'
traverseMap = index $ \f -> sequenceA . mapWithKey f

-- | Traverse the value at a given key in a Map
--
-- > traverseAt :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
-- > traverseAt k = valueAt k . traverse
traverseAt :: Ord k => k -> SimpleTraversal (Map k v) v
traverseAt k = at k . traverse
{-# INLINE traverseAt #-}

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
