{-# LANGUAGE MagicHash #-}
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

import Control.Lens.Classes
import Control.Lens.Indexed
import Control.Lens.Internal
import Control.Lens.Internal.Combinators
import Control.Lens.Type
import Control.Monad.State.Class as State

infixr 4 %@~
infix  4 %@=

-- | Every 'IndexedSetter' is a valid 'Setter'
--
-- The 'Control.Lens.Setter.Setter' laws are still required to hold.
type IndexedSetter i s t a b = forall f k. (Indexed i k, Settable f) => k (a -> f b) (s -> f t)

-- |
-- @type 'SimpleIndexedSetter' i = 'Simple' ('IndexedSetter' i)@
type SimpleIndexedSetter i s a = IndexedSetter i s s a a

-- | Map with index.
--
-- When you do not need access to the index, then 'mapOf' is more liberal in what it can accept.
--
-- @'Control.Lens.Setter.mapOf' l ≡ 'imapOf' l '.' 'const'@
--
-- @
-- 'imapOf' :: 'IndexedSetter' i s t a b    -> (i -> a -> b) -> s -> t
-- 'imapOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> a -> b) -> s -> t
-- 'imapOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> t
-- @
imapOf :: Overloaded (Index i) Mutator s t a b -> (i -> a -> b) -> s -> t
imapOf l f = runMutator# (withIndex l (\i -> mutator# (f i)))
{-# INLINE imapOf #-}

-- | Map with index. This is an alias for 'imapOf'.
--
-- When you do not need access to the index, then 'over' is more liberal in what it can accept.
--
-- @'Control.Lens.Setter.over' l ≡ 'iover' l '.' 'const'@
--
-- @
-- 'iover' :: 'IndexedSetter' i s t a b    -> (i -> a -> b) -> s -> t
-- 'iover' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> a -> b) -> s -> t
-- 'iover' :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> t
-- @
iover :: Overloaded (Index i) Mutator s t a b -> (i -> a -> b) -> s -> t
iover l f = runMutator# (withIndex l (\i -> mutator# (f i)))
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
isets :: ((i -> a -> b) -> s -> t) -> IndexedSetter i s t a b
isets f = index $ \ g -> tainted# (f (\i -> untainted# (g i)))
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
-- ('%@~') :: 'IndexedSetter' i s t a b    -> (i -> a -> b) -> s -> t
-- ('%@~') :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> a -> b) -> s -> t
-- ('%@~') :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> t
-- @
(%@~) :: Overloaded (Index i) Mutator s t a b -> (i -> a -> b) -> s -> t
l %@~ f = runMutator# (withIndex l (\i -> mutator# (f i)))
{-# INLINE (%@~) #-}

-- | Adjust every target in the current state of an 'IndexedSetter', 'Control.Lens.IndexedLens.IndexedLens' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- When you do not need access to the index then ('Control.Lens.Setter.%=') is more liberal in what it can accept.
--
-- @l 'Control.Lens.Setter.%=' f ≡ l '%@=' 'const' f@
--
-- @
-- ('%@=') :: 'MonadState' s m => 'IndexedSetter' i s s a b    -> (i -> a -> b) -> m ()
-- ('%@=') :: 'MonadState' s m => 'Control.Lens.IndexedLens.IndexedLens' i s s a b      -> (i -> a -> b) -> m ()
-- ('%@=') :: 'MonadState' s m => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> m ()
-- @
(%@=) :: MonadState s m => Overloaded (Index i) Mutator s s a b -> (i -> a -> b) -> m ()
l %@= f = State.modify (l %@~ f)
{-# INLINE (%@=) #-}

------------------------------------------------------------------------------
-- Reifying Indexed Setters
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedSetter i s t a b = ReifyIndexedSetter { reflectIndexedSetter :: IndexedSetter i s t a b }

-- | @type 'SimpleIndexedSetter' i = 'Simple' ('ReifiedIndexedSetter' i)@
type SimpleReifiedIndexedSetter i s a = ReifiedIndexedSetter i s s a a

