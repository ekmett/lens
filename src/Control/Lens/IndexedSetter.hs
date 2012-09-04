{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedSetter
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.IndexedSetter
  (
  -- * Indexed Setter
    IndexedSetter
  , imapOf, iover
  , isets
  , (%@~)
  , (%@=)
  -- * Storing Indexed Setters
  , ReifiedIndexedSetter(..)
  -- * Simple
  , SimpleIndexedSetter
  , SimpleReifiedIndexedSetter
  ) where

import Control.Applicative
import Control.Lens.Indexed
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.State.Class as State

infixr 4 %@~
infix  4 %@=

-- | Every 'IndexedSetter' is a valid 'Setter'
--
-- The 'Control.Lens.Setter.Setter' laws are still required to hold.
type IndexedSetter i a b c d = forall f k. (Indexed i k, Settable f) => k (c -> f d) (a -> f b)

-- |
-- @type 'SimpleIndexedSetter' i = 'Simple' ('IndexedSetter' i)@
type SimpleIndexedSetter i a b = IndexedSetter i a a b b

-- | Map with index.
--
-- When you do not need access to the index, then 'mapOf' is more liberal in what it can accept.
--
-- @'Control.Lens.Setter.mapOf' l ≡ 'imapOf' l '.' 'const'@
--
-- @
-- 'imapOf' :: 'IndexedSetter' i a b c d    -> (i -> c -> d) -> a -> b
-- 'imapOf' :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> d) -> a -> b
-- 'imapOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> a -> b
-- @
imapOf :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
imapOf l f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE imapOf #-}

-- | Map with index. This is an alias for 'imapOf'.
--
-- When you do not need access to the index, then 'over' is more liberal in what it can accept.
--
-- @'Control.Lens.Setter.over' l ≡ 'iover' l '.' 'const'@
--
-- @
-- 'iover' :: 'IndexedSetter' i a b c d    -> (i -> c -> d) -> a -> b
-- 'iover' :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> d) -> a -> b
-- 'iover' :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> a -> b
-- @
iover :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
iover l f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE iover #-}

-- | Build an 'IndexedSetter' from an 'imap'-like function.
--
-- Your supplied function @f@ is required to satisfy:
--
-- @
-- f 'id' ≡ 'id'
-- f g '.' f h ≡ f (g '.' h)
-- @
--
-- Equational reasoning:
--
-- @
-- 'isets' '.' 'iover' ≡ 'id'
-- 'iover' '.' 'isets' ≡ 'id'
-- @
--
-- Another way to view 'sets' is that it takes a \"semantic editor combinator\"
-- and transforms it into a 'Setter'.
isets :: ((i -> c -> d) -> a -> b) -> IndexedSetter i a b c d
isets f = index $ \ g -> pure . f (\i -> untainted . g i)
{-# INLINE isets #-}

-- | Adjust every target of an 'IndexedSetter', 'Control.Lens.IndexedLens.IndexedLens' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- @('%@~') ≡ 'imapOf'@
--
-- When you do not need access to the index then ('%@~') is more liberal in what it can accept.
--
-- @l 'Control.Lens.Setter.%~' f ≡ l '%@~' 'const' f@
--
-- @
-- ('%@~') :: 'IndexedSetter' i a b c d    -> (i -> c -> d) -> a -> b
-- ('%@~') :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> d) -> a -> b
-- ('%@~') :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> a -> b
-- @
(%@~) :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
l %@~ f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE (%@~) #-}

-- | Adjust every target in the current state of an 'IndexedSetter', 'Control.Lens.IndexedLens.IndexedLens' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- When you do not need access to the index then ('Control.Lens.Setter.%=') is more liberal in what it can accept.
--
-- @l 'Control.Lens.Setter.%=' f ≡ l '%@=' 'const' f@
--
-- @
-- ('%@=') :: 'MonadState' a m => 'IndexedSetter' i a a c d    -> (i -> c -> d) -> m ()
-- ('%@=') :: 'MonadState' a m => 'Control.Lens.IndexedLens.IndexedLens' i a a c d      -> (i -> c -> d) -> m ()
-- ('%@=') :: 'MonadState' a m => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> m ()
-- @
(%@=) :: MonadState a m => Overloaded (Index i) Mutator a a c d -> (i -> c -> d) -> m ()
l %@= f = State.modify (l %@~ f)
{-# INLINE (%@=) #-}

------------------------------------------------------------------------------
-- Reifying Indexed Setters
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedSetter i a b c d = ReifyIndexedSetter { reflectIndexedSetter :: IndexedSetter i a b c d }

-- | @type 'SimpleIndexedSetter' i = 'Simple' ('ReifiedIndexedSetter' i)@
type SimpleReifiedIndexedSetter i a b = ReifiedIndexedSetter i a a b b

