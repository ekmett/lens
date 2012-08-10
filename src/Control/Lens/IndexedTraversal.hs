{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Simple
  , SimpleIndexedTraversal
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Indexed
import Control.Lens.Type
import Control.Monad.Trans.State.Lazy as Lazy

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
-- 'itraverseOf' = 'withIndex'
-- 'Control.Lens.Traversal.traverseOf' = 'itraverseOf' . 'const' = 'id'
-- @
--
-- @
-- itraverseOf :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> f d) -> a -> f b
-- itraverseOf :: 'IndexedTraversal' i a b c d -> (i -> c -> f d) -> a -> f b
-- @
itraverseOf :: Overloaded (Index i) f a b c d -> (i -> c -> f d) -> a -> f b
itraverseOf = withIndex
{-# INLINE itraverseOf #-}

-- |
-- Traverse with an index (and the arguments flipped)
--
-- @'Control.Lens.Traversal.forOf' l a = 'iforOf' l a . 'const'@
--
-- @'iforOf' = 'flip' . 'itraverseOf'@
--
-- @
-- iforOf :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> a -> (i -> c -> f d) -> f b
-- iforOf :: 'IndexedTraversal' i a b c d -> a -> (i -> c -> f d) -> f b
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
-- @'Control.Lens.Traversal.mapMOf' = 'imapMOf' . 'const'@
--
-- @
-- imapMOf :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'      i a b c d -> (i -> c -> m d) -> a -> m b
-- imapMOf :: 'Monad' m => 'IndexedTraversal' i a b c d -> (i -> c -> m d) -> a -> m b
-- @
imapMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> (i -> c -> m d) -> a -> m b
imapMOf l f = unwrapMonad . withIndex l (\i -> WrapMonad . f i)
{-# INLINE imapMOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'Control.Lens.Traversal.forMOf' l a = 'iforMOf' l a . 'const'
-- 'iforMOf' = 'flip' . 'imapMOf'
-- @
--
-- @
-- iforMOf :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> a -> (i -> c -> m d) -> m b
-- iforMOf :: 'Monad' m => 'IndexedTraversal' i a b c d -> a -> (i -> c -> m d) -> m b
-- @
iforMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> a -> (i -> c -> m d) -> m b
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @'Control.Lens.Traversal.mapAccumROf' l = 'imapAccumROf' l . 'const'@
--
-- @
-- imapAccumROf :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- imapAccumROf :: 'IndexedTraversal' i a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
imapAccumROf :: Overloaded (Index i) (Lazy.State s) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumROf l f s0 a = swap (Lazy.runState (withIndex l (\i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @'Control.Lens.Traversal.mapAccumLOf' l = 'imapAccumLOf' l . 'const'@
--
-- @
-- imapAccumLOf :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- imapAccumLOf :: 'IndexedTraversal' i a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
imapAccumLOf :: Overloaded (Index i) (Backwards (Lazy.State s)) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumLOf l f s0 a = swap (Lazy.runState (forwards (withIndex l (\i c -> Backwards (Lazy.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

