{-# LANGUAGE Rank2Types #-}
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
  ( at
  , traverseAt
  , traverseAtMin
  , traverseAtMax
  ) where

import Control.Applicative as Applicative
import Control.Lens.Type
import Control.Lens.Traversal
import Data.Map as Map

-- | This 'Lens' can be used to read, write or delete the value associated with a key in a 'Map'.
--
-- > ghci> Map.fromList [("hello",12)] ^.at "hello"
-- > Just 12
--
-- > at :: Ord k => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
at :: Ord k => k -> SimpleLens (Map k v) (Maybe v)
at k f m = go <$> f (Map.lookup k m) where
  go Nothing   = Map.delete k m
  go (Just v') = Map.insert k v' m
{-# INLINE at #-}

-- | Traverse the value at a given key in a Map
--
-- > traverseAt :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
-- > traverseAt k = valueAt k . traverse
traverseAt :: Ord k => k -> SimpleTraversal (Map k v) v
traverseAt k = at k . traverse
{-# INLINE traverseAt #-}

-- | Traverse the value at the minimum key in a Map
traverseAtMin :: SimpleTraversal (Map k v) v
traverseAtMin f m = case Map.minView m of
  Just (a, _) -> (\v -> Map.updateMin (const (Just v)) m) <$> f a
  Nothing     -> pure m
{-# INLINE traverseAtMin #-}

-- | Traverse the value at the maximum key in a Map
traverseAtMax :: SimpleTraversal (Map k v) v
traverseAtMax f m = case Map.maxView m of
    Just (a, _) -> (\v -> Map.updateMax (const (Just v)) m) <$> f a
    Nothing     -> pure m
{-# INLINE traverseAtMax #-}
