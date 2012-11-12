{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

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

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Every indexed traversal is a valid 'Control.Lens.Traversal.Traversal' or 'Control.Lens.IndexedFold.IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'IndexedTraversal' to be used directly as a 'Control.Lens.Traversal.Traversal'.
--
-- The 'Control.Lens.Traversal.Traversal' laws are still required to hold.
type IndexedTraversal i s t a b = forall f k. (Indexed i k, Applicative f) => k (a -> f b) (s -> f t)

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i s a = IndexedTraversal i s s a a

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
-- 'itraverseOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- 'itraverseOf' :: 'IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
itraverseOf :: Overloaded (Index i) f s t a b -> (i -> a -> f b) -> s -> f t
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
-- 'iforOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> s -> (i -> a -> f b) -> f t
-- 'iforOf' :: 'IndexedTraversal' i s t a b -> s -> (i -> a -> f b) -> f t
-- @
iforOf :: Overloaded (Index i) f s t a b -> s -> (i -> a -> f b) -> f t
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
-- 'imapMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'      i s t a b -> (i -> a -> m b) -> s -> m t
-- 'imapMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> (i -> a -> m b) -> s -> m t
-- @
imapMOf :: Overloaded (Index i) (WrappedMonad m) s t a b -> (i -> a -> m b) -> s -> m t
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
-- 'iforMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> s -> (i -> a -> m b) -> m t
-- 'iforMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> s -> (i -> a -> m b) -> m t
-- @
iforMOf :: Overloaded (Index i) (WrappedMonad m) s t a b -> s -> (i -> a -> m b) -> m t
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @'Control.Lens.Traversal.mapAccumROf' l ≡ 'imapAccumROf' l '.' 'const'@
--
-- @
-- 'imapAccumROf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- 'imapAccumROf' :: 'IndexedTraversal' i s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- @
imapAccumROf :: Overloaded (Index i) (Lazy.State s) s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
imapAccumROf l f s0 a = swap (Lazy.runState (withIndex l (\i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @'Control.Lens.Traversal.mapAccumLOf' l ≡ 'imapAccumLOf' l '.' 'const'@
--
-- @
-- 'imapAccumLOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- 'imapAccumLOf' :: 'IndexedTraversal' i s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- @
imapAccumLOf :: Overloaded (Index i) (Backwards (Lazy.State s)) s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
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
-- >>> over (iwhereOf (indexed traverse) (>0)) reverse $ ["He","was","stressed","o_O"]
-- ["He","saw","desserts","O_o"]
--
-- @
-- 'iwhereOf' :: 'IndexedFold' i s a            -> (i -> 'Bool') -> 'IndexedFold' i s a
-- 'iwhereOf' :: 'IndexedGetter' i s a          -> (i -> 'Bool') -> 'IndexedFold' i s a
-- 'iwhereOf' :: 'SimpleIndexedLens' i s a      -> (i -> 'Bool') -> 'SimpleIndexedTraversal' i s a
-- 'iwhereOf' :: 'SimpleIndexedTraversal' i s a -> (i -> 'Bool') -> 'SimpleIndexedTraversal' i s a
-- 'iwhereOf' :: 'SimpleIndexedSetter' i s a    -> (i -> 'Bool') -> 'SimpleIndexedSetter' i s a
-- @
iwhereOf :: (Indexed i k, Applicative f) => Overloaded (Index i) f s t a a -> (i -> Bool) -> Overloaded k f s t a a
iwhereOf l p = index $ \f s -> withIndex l (\i a -> if p i then f i a else pure a) s
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
newtype ReifiedIndexedTraversal i s t a b = ReifyIndexedTraversal { reflectIndexedTraversal :: IndexedTraversal i s t a b }

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('ReifiedIndexedTraversal' i)@
type SimpleReifiedIndexedTraversal i s a = ReifiedIndexedTraversal i s s a a
