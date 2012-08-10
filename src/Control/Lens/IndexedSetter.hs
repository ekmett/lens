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
  , imapOf
  , (%@~)
  , (%@=)
  -- * Simple
  , SimpleIndexedSetter
  ) where

import Control.Lens.Indexed
import Control.Lens.Setter
import Control.Lens.Type
import Control.Monad.State.Class as State

infixr 4 %@~
infix  4 %@=

-- | Every 'IndexedSetter' is a valid 'Setter'
--
-- The 'Setter' laws are still required to hold.
type IndexedSetter i a b c d = forall f k. (Indexed i k, Settable f) => k (c -> f d) (a -> f b)

-- |
-- @type 'SimpleIndexedSetter' i = 'Simple' ('IndexedSetter' i)@
type SimpleIndexedSetter i a b = IndexedSetter i a a b b

-- | Map with index.
--
-- When you do not need access to the index, then 'mapOf' is more liberal in what it can accept.
--
-- @'mapOf' l = 'imapOf' l . 'const'@
--
-- @
-- imapOf :: 'IndexedSetter' i a b c d    -> (i -> c -> d) -> a -> b
-- imapOf :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> d) -> a -> b
-- imapOf :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> a -> b
-- @
imapOf :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
imapOf l f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE imapOf #-}

-- | Adjust every target of an 'IndexedSetter', 'Control.Lens.IndexedLens.IndexedLens' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- @('%@~') = 'imapOf'@
--
-- When you do not need access to the index then ('%@~') is more liberal in what it can accept.
--
-- @l '%~' f = l '%@~' 'const' f@
--
-- @
-- (%@~) :: 'IndexedSetter' i a b c d    -> (i -> c -> d) -> a -> b
-- (%@~) :: 'Control.Lens.IndexedLens.IndexedLens' i a b c d      -> (i -> c -> d) -> a -> b
-- (%@~) :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> a -> b
-- @
(%@~) :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
l %@~ f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE (%@~) #-}

-- | Adjust every target in the current state of an 'IndexedSetter', 'Control.Lens.IndexedLens.IndexedLens' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- When you do not need access to the index then ('%=') is more liberal in what it can accept.
--
-- @l '%=' f = l '%@=' 'const' f@
--
-- @
-- (%@=) :: 'MonadState' a m => 'IndexedSetter' i a a c d    -> (i -> c -> d) -> m ()
-- (%@=) :: 'MonadState' a m => 'Control.Lens.IndexedLens.IndexedLens' i a a c d      -> (i -> c -> d) -> m ()
-- (%@=) :: 'MonadState' a m => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> m ()
-- @
(%@=) :: MonadState a m => Overloaded (Index i) Mutator a a c d -> (i -> c -> d) -> m ()
l %@= f = State.modify (l %@~ f)
{-# INLINE (%@=) #-}
