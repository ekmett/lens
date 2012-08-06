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
  , composeWithIndex
  , reindex

  -- * Indexed Folds
  , IndexedFold
  , foldMapWithIndexOf
  , foldrWithIndexOf

  -- * Indexed Traversals
  , IndexedTraversal
  , traverseWithIndexOf
  , mapMWithIndexOf

  -- * Indexed Setter
  , IndexedSetter
  , mapWithIndexOf
  , (%@)

  -- * Simple
  , SimpleIndexedTraversal
  , SimpleIndexedSetter
  ) where

import Control.Applicative
import Control.Lens.Type
import Control.Lens.Getter
import Control.Lens.Setter
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
f .@ g = composeWithIndex (,) f g
{-# INLINE (.@) #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> Index (i,j) a c #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> a -> c #-}

-- | Composition of indexed functions with a user supplied function for combining indexs
composeWithIndex :: Indexed k r => (i -> j -> k) -> Index i b c -> Index j a b -> r a c
composeWithIndex ijk (Index ibc) (Index jab) = index $ \ka -> ibc $ \i -> jab $ \j -> ka (ijk i j)
{-# INLINE composeWithIndex #-}
{-# SPECIALIZE composeWithIndex :: (i -> j -> k) -> Index i b c -> Index j a b -> a -> c #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Every 'IndexedFold' is a valid 'Fold'
type IndexedFold i a c = forall k f b d. (Indexed i k, Applicative f, Gettable f) => k (c -> f d) (a -> f b)

type IndexedFolding i m a b c d = Index i (c -> Accessor m d) (a -> Accessor m b)

-- |
--
-- > foldMapWithIndexOf :: Monoid m => IndexedFold i a c          -> (i -> c -> m) -> a -> m
-- > foldMapWithIndexOf :: Monoid m => IndexedTraversal i a b c d -> (i -> c -> m) -> a -> m
foldMapWithIndexOf :: IndexedFolding i m a b c d -> (i -> c -> m) -> a -> m
foldMapWithIndexOf l f = runAccessor . withIndex l (\i -> Accessor . f i)
{-# INLINE foldMapWithIndexOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- > foldrWithIndexOf :: IndexedFold i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- > foldrWithIndexOf :: IndexedTraversal i a b c d -> (i -> c -> e -> e) -> e -> a -> e
foldrWithIndexOf :: IndexedFolding i (Endo e) a b c d -> (i -> c -> e -> e) -> e -> a -> e
foldrWithIndexOf l f z t = appEndo (foldMapWithIndexOf l (\i -> Endo . f i) t) z
{-# INLINE foldrWithIndexOf #-}

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Every indexed traversal is a valid Traversal or indexed fold.
--
-- The Traversal laws are still required to hold. Moreover, each index should be distinct.
type IndexedTraversal i a b c d = forall f k. (Indexed i k, Applicative f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i a b = IndexedTraversal i a a b b

-- |
-- > traverseWithIndexOf :: IndexedTraversal i a b c d -> (i -> c -> f d) -> a -> f b
traverseWithIndexOf :: Overloaded (Index i) f a b c d -> (i -> c -> f d) -> a -> f b
traverseWithIndexOf = withIndex
{-# INLINE traverseWithIndexOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- > mapMWithIndexOf :: Monad m => IndexedTraversal a b c d -> (i -> c -> m d) -> a -> m b
mapMWithIndexOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> (i -> c -> m d) -> a -> m b
mapMWithIndexOf l f = unwrapMonad . withIndex l (\i -> WrapMonad . f i)
{-# INLINE mapMWithIndexOf #-}

-- | Every indexed Setter is a valid Setter
--
-- The Setter laws are still required to hold. Moreover, each index should be distinct.
type IndexedSetter i a b c d = forall f k. (Indexed i k, Settable f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedSetter i a b = IndexedSetter i a a b b

-- | Map with index
--
-- > mapWithIndexOf :: IndexedSetter i a b c d -> (i -> c -> d) -> a -> b
mapWithIndexOf :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
mapWithIndexOf l f = runMutator . withIndex l (\i -> Mutator . f i)

infixr 4 %@

-- | > (%@) = mapWithIndexOf
(%@) :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
l %@ f = runMutator . withIndex l (\i -> Mutator . f i)

