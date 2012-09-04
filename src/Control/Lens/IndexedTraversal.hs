{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedTraversal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.IndexedTraversal
  (
  -- * Indexed Traversals
    IndexedTraversal

  -- * Common Indexed Traversals
  , traverseAt
  , iwhereOf
  , value
  , TraverseMin(..)
  , TraverseMax(..)

  -- * Indexed Traversal Combinators
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Storing Indexed Traversals
  , ReifiedIndexedTraversal(..)

  -- * Simple
  , SimpleIndexedTraversal
  , SimpleReifiedIndexedTraversal
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.Type
import Control.Monad.Trans.State.Lazy as Lazy
import Data.Traversable
import Data.IntMap as IntMap
import Data.Map as Map

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Every indexed traversal is a valid 'Control.Lens.Traversal.Traversal' or 'Control.Lens.IndexedFold.IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'IndexedTraversal' to be used directly as a 'Control.Lens.Traversal.Traversal'.
--
-- The 'Control.Lens.Traversal.Traversal' laws are still required to hold.
type IndexedTraversal i a b c d = forall f k. (Indexed i k, Applicative f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i a b = IndexedTraversal i a a b b

-- | Traversal with an index.
--
-- NB: When you don't need access to the index then you can just apply your 'IndexedTraversal'
-- directly as a function!
--
-- @
-- 'itraverseOf' ≡ 'withIndex'
-- 'Control.Lens.Traversal.traverseOf' l = 'itraverseOf' l '.' 'const' = 'id'
-- @
--
-- @
-- 'itraverseOf' :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> f d) -> a -> f b
-- 'itraverseOf' :: 'IndexedTraversal' i a b c d -> (i -> c -> f d) -> a -> f b
-- @
itraverseOf :: Overloaded (Index i) f a b c d -> (i -> c -> f d) -> a -> f b
itraverseOf = withIndex
{-# INLINE itraverseOf #-}

-- |
-- Traverse with an index (and the arguments flipped)
--
-- @
-- 'Control.Lens.Traversal.forOf' l a ≡ 'iforOf' l a '.' 'const'
-- 'iforOf' ≡ 'flip' . 'itraverseOf'
-- @
--
-- @
-- 'iforOf' :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> a -> (i -> c -> f d) -> f b
-- 'iforOf' :: 'IndexedTraversal' i a b c d -> a -> (i -> c -> f d) -> f b
-- @
iforOf :: Overloaded (Index i) f a b c d -> a -> (i -> c -> f d) -> f b
iforOf = flip . withIndex
{-# INLINE iforOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- When you don't need access to the index 'mapMOf' is more liberal in what it can accept.
--
-- @'Control.Lens.Traversal.mapMOf' l ≡ 'imapMOf' l '.' 'const'@
--
-- @
-- 'imapMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'      i a b c d -> (i -> c -> m d) -> a -> m b
-- 'imapMOf' :: 'Monad' m => 'IndexedTraversal' i a b c d -> (i -> c -> m d) -> a -> m b
-- @
imapMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> (i -> c -> m d) -> a -> m b
imapMOf l f = unwrapMonad . withIndex l (\i -> WrapMonad . f i)
{-# INLINE imapMOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'Control.Lens.Traversal.forMOf' l a ≡ 'iforMOf' l a '.' 'const'
-- 'iforMOf' ≡ 'flip' '.' 'imapMOf'
-- @
--
-- @
-- 'iforMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> a -> (i -> c -> m d) -> m b
-- 'iforMOf' :: 'Monad' m => 'IndexedTraversal' i a b c d -> a -> (i -> c -> m d) -> m b
-- @
iforMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> a -> (i -> c -> m d) -> m b
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @'Control.Lens.Traversal.mapAccumROf' l ≡ 'imapAccumROf' l '.' 'const'@
--
-- @
-- 'imapAccumROf' :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- 'imapAccumROf' :: 'IndexedTraversal' i a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
imapAccumROf :: Overloaded (Index i) (Lazy.State s) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumROf l f s0 a = swap (Lazy.runState (withIndex l (\i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @'Control.Lens.Traversal.mapAccumLOf' l ≡ 'imapAccumLOf' l '.' 'const'@
--
-- @
-- 'imapAccumLOf' :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- 'imapAccumLOf' :: 'IndexedTraversal' i a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
imapAccumLOf :: Overloaded (Index i) (Backwards (Lazy.State s)) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumLOf l f s0 a = swap (Lazy.runState (forwards (withIndex l (\i c -> Backwards (Lazy.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

------------------------------------------------------------------------------
-- Common Indexed Traversals
------------------------------------------------------------------------------

-- | Access the element of an 'IndexedTraversal' where the index matches a predicate.
--
-- >>> :m + Control.Lens
-- >>> over (iwhereOf (indexed traverse) (>0)) reverse $ ["He","was","stressed","o_O"]
-- ["He","saw","desserts","O_o"]
--
-- @
-- 'iwhereOf' :: 'IndexedFold' i a b            -> (i -> 'Bool') -> 'IndexedFold' i a b
-- 'iwhereOf' :: 'IndexedGetter' i a b          -> (i -> 'Bool') -> 'IndexedFold' i a b
-- 'iwhereOf' :: 'SimpleIndexedLens' i a b      -> (i -> 'Bool') -> 'SimpleIndexedTraversal' i a b
-- 'iwhereOf' :: 'SimpleIndexedTraversal' i a b -> (i -> 'Bool') -> 'SimpleIndexedTraversal' i a b
-- 'iwhereOf' :: 'SimpleIndexedSetter' i a b    -> (i -> 'Bool') -> 'SimpleIndexedSetter' i a b
-- @
iwhereOf :: (Indexed i k, Applicative f) => Overloaded (Index i) f a b c c -> (i -> Bool) -> Overloaded k f a b c c
iwhereOf l p = index $ \f a -> withIndex l (\i c -> if p i then f i c else pure c) a
{-# INLINE iwhereOf #-}

-- | Traverse the value at a given key in a map
--
-- @'traverseAt' k = 'at' k '<.' 'traverse'@
traverseAt :: At k m => k -> SimpleIndexedTraversal k (m v) v
traverseAt k = at k <. traverse
{-# INLINE traverseAt #-}

-- | This provides a 'Traversal' that checks a predicate on a key before
-- allowing you to traverse into a value.
value :: (k -> Bool) -> SimpleIndexedTraversal k (k, v) v
value p = index $ \ f kv@(k,v) -> if p k then (,) k <$> f k v else pure kv
{-# INLINE value #-}

-- | Allows 'IndexedTraversal' the value at the smallest index.
class Ord k => TraverseMin k m | m -> k where
  -- | 'IndexedTraversal' of the element with the smallest index.
  traverseMin :: SimpleIndexedTraversal k (m v) v

instance TraverseMin Int IntMap where
  traverseMin = index $ \f m -> case IntMap.minViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> (\v -> IntMap.updateMin (const (Just v)) m) <$> f k a
#else
    Just ((k,a), _) -> (\v -> IntMap.updateMin (const v) m) <$> f k a
#endif
    Nothing     -> pure m
  {-# INLINE traverseMin #-}

instance Ord k => TraverseMin k (Map k) where
  traverseMin = index $ \f m -> case Map.minViewWithKey m of
    Just ((k, a), _) -> (\v -> Map.updateMin (const (Just v)) m) <$> f k a
    Nothing          -> pure m
  {-# INLINE traverseMin #-}

-- | Allows 'IndexedTraversal' of the value at the largest index.
class Ord k => TraverseMax k m | m -> k where
  -- | 'IndexedTraversal' of the element at the largest index.
  traverseMax :: SimpleIndexedTraversal k (m v) v

instance TraverseMax Int IntMap where
  traverseMax = index $ \f m -> case IntMap.maxViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> (\v -> IntMap.updateMax (const (Just v)) m) <$> f k a
#else
    Just ((k,a), _) -> (\v -> IntMap.updateMax (const v) m) <$> f k a
#endif
    Nothing     -> pure m
  {-# INLINE traverseMax #-}

instance Ord k => TraverseMax k (Map k) where
  traverseMax = index $ \f m -> case Map.maxViewWithKey m of
    Just ((k, a), _) -> (\v -> Map.updateMax (const (Just v)) m) <$> f k a
    Nothing          -> pure m
  {-# INLINE traverseMax #-}

------------------------------------------------------------------------------
-- Reifying Indexed Traversals
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedTraversal i a b c d = ReifyIndexedTraversal { reflectIndexedTraversal :: IndexedTraversal i a b c d }

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('ReifiedIndexedTraversal' i)@
type SimpleReifiedIndexedTraversal i a b = ReifiedIndexedTraversal i a a b b
