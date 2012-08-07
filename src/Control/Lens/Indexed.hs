{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Indexed
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.Indexed
  (
  -- * Indexed Functions
    Indexed(..)
  , Indexable
  , Index(..)
  , (.@)
  , icompose
  , reindex

  -- * Indexed Folds
  , IndexedFold
  , ifoldMapOf
  , ifoldrOf

  -- * Indexed Traversals
  , IndexedTraversal
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Indexed Setter
  , IndexedSetter
  , imapOf
  , (%@)

  -- * Simple
  , SimpleIndexedTraversal
  , SimpleIndexedSetter
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Type
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Monad.State.Class as State
import Control.Monad.Trans.State.Lazy as Lazy
import Data.Monoid

-- | Permit overloading of function application for things that also admit a notion of a key or index.

-- | Provides overloading for indexed functions.
class Indexed i k where
  -- | Build a function from an indexed function
  index :: ((i -> a) -> b) -> k a b

-- | Type alias for passing around polymorphic indexed functions.
type Indexable i a b = forall k. Indexed i k => k a b

instance Indexed i (->) where
  index f = f . const
  {-# INLINE index #-}

-- | A function with access to a index. This constructor may be useful when you need to store
-- a 'HasIndex'.
newtype Index i a b = Index { withIndex :: (i -> a) -> b }

-- | Using an equality witness to avoid potential overlapping instances
-- and aid dispatch.
instance i ~ j => Indexed i (Index j) where
  index = Index
  {-# INLINE index #-}

-- | Remap the index.
reindex :: Indexed j k => (i -> j) -> Index i a b -> k a b
reindex ij (Index iab) = index $ \ ja -> iab $ \i -> ja (ij i)
{-# SPECIALIZE reindex :: (i -> j) -> Index i a b -> Index j a b #-}
{-# SPECIALIZE reindex :: (i -> j) -> Index i a b -> a -> b #-}

infixr 9 .@
-- | Composition of indexed functions
(.@) :: Indexed (i, j) k => Index i b c -> Index j a b -> k a c
f .@ g = icompose (,) f g
{-# INLINE (.@) #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> Index (i,j) a c #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> a -> c #-}

-- | Composition of indexed functions with a user supplied function for combining indexs
icompose :: Indexed k r => (i -> j -> k) -> Index i b c -> Index j a b -> r a c
icompose ijk (Index ibc) (Index jab) = index $ \ka -> ibc $ \i -> jab $ \j -> ka (ijk i j)
{-# INLINE icompose #-}
{-# SPECIALIZE icompose :: (i -> j -> k) -> Index i b c -> Index j a b -> a -> c #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Every 'IndexedFold' is a valid 'Fold'.
type IndexedFold i a c = forall k f b d. (Indexed i k, Applicative f, Gettable f) => k (c -> f d) (a -> f b)

type IndexedFolding i m a b c d = Index i (c -> Accessor m d) (a -> Accessor m b)

-- |
--
-- > ifoldMapOf :: Monoid m => IndexedFold i a c          -> (i -> c -> m) -> a -> m
-- > ifoldMapOf :: Monoid m => IndexedTraversal i a b c d -> (i -> c -> m) -> a -> m
ifoldMapOf :: IndexedFolding i m a b c d -> (i -> c -> m) -> a -> m
ifoldMapOf l f = runAccessor . withIndex l (\i -> Accessor . f i)
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- > ifoldrOf :: IndexedFold i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf :: IndexedTraversal i a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf :: IndexedFolding i (Endo e) a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf l f z t = appEndo (ifoldMapOf l (\i -> Endo . f i) t) z
{-# INLINE ifoldrOf #-}

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Every indexed traversal is a valid Traversal or indexed fold.
--
-- The Traversal laws are still required to hold.
type IndexedTraversal i a b c d = forall f k. (Indexed i k, Applicative f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i a b = IndexedTraversal i a a b b

-- | Traversal with an index.
--
-- > itraverseOf = withIndex
--
-- > itraverseOf :: IndexedTraversal i a b c d -> (i -> c -> f d) -> a -> f b
itraverseOf :: Overloaded (Index i) f a b c d -> (i -> c -> f d) -> a -> f b
itraverseOf = withIndex
{-# INLINE itraverseOf #-}

-- |
-- > iforOf = flip . itraverseOf
iforOf :: Overloaded (Index i) f a b c d -> a -> (i -> c -> f d) -> f b
iforOf = flip . withIndex
{-# INLINE iforOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- > imapMOf :: Monad m => IndexedTraversal a b c d -> (i -> c -> m d) -> a -> m b
imapMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> (i -> c -> m d) -> a -> m b
imapMOf l f = unwrapMonad . withIndex l (\i -> WrapMonad . f i)
{-# INLINE imapMOf #-}

-- |
-- > iforMOf = flip . imapMOf
iforMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> a -> (i -> c -> m d) -> m b
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal'.
--
-- 'imapAccumROf' accumulates state from right to left.
--
imapAccumROf :: Overloaded (Index i) (Lazy.State s) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumROf l f s0 a = swap (Lazy.runState (withIndex l (\i c -> State.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalized 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal'.
--
-- 'imapAccumLOf' accumulates state from left to right.
imapAccumLOf :: Overloaded (Index i) (Backwards (Lazy.State s)) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumLOf l f s0 a = swap (Lazy.runState (forwards (withIndex l (\i c -> Backwards (State.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

-- | Every indexed Setter is a valid Setter
--
-- The Setter laws are still required to hold.
type IndexedSetter i a b c d = forall f k. (Indexed i k, Settable f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedSetter i a b = IndexedSetter i a a b b

-- | Map with index
--
-- > imapOf :: IndexedSetter i a b c d -> (i -> c -> d) -> a -> b
imapOf :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
imapOf l f = runMutator . withIndex l (\i -> Mutator . f i)

infixr 4 %@

-- | > (%@) = imapOf
(%@) :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
l %@ f = runMutator . withIndex l (\i -> Mutator . f i)

