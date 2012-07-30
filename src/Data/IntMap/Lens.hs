{-# LANGUAGE Rank2Types #-}
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
  , traverseAt
  , traverseAtMin
  , traverseAtMax
  ) where

import Control.Applicative as Applicative
import Control.Lens
import Data.IntMap as IntMap

-- | This 'Lens' can be used to read, write or delete the value associated with a key in an 'IntMap'.
--
-- > ghci> fromList [(1,"hello")] ^.at 1
-- > Just "hello"
--
-- > ghci> at 1 ^~ Just "hello" $ mempty
-- > fromList [(1,"hello")]
--
-- > at :: Int -> (Maybe v -> f (Maybe v)) -> IntMap v -> f (IntMap v)
at :: Int -> Simple Lens (IntMap v) (Maybe v)
at k f m = go <$> f (IntMap.lookup k m) where
  go Nothing   = IntMap.delete k m
  go (Just v') = IntMap.insert k v' m
{-# INLINE at #-}

-- | Traverse the value at a given key in an IntMap
--
-- > traverseAt :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
-- > traverseAt k = at k . traverse
traverseAt :: Int -> Simple Traversal (IntMap v) v
traverseAt k = at k . traverse
{-# INLINE traverseAt #-}

-- | Traverse the value at the minimum key in a Map
traverseAtMin :: Simple Traversal (IntMap v) v
traverseAtMin f m = case IntMap.minView m of
  Just (a, _) -> (\v -> IntMap.updateMin (const v) m) <$> f a
  Nothing     -> pure m
{-# INLINE traverseAtMin #-}

-- | Traverse the value at the maximum key in a Map
traverseAtMax :: Simple Traversal (IntMap v) v
traverseAtMax f m = case IntMap.maxView m of
    Just (a, _) -> (\v -> IntMap.updateMax (const v) m) <$> f a
    Nothing     -> pure m
{-# INLINE traverseAtMax #-}
