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
-- Combinators for working with 'Indexed' functions.
----------------------------------------------------------------------------
module Control.Lens.Indexed
  (
  -- * Indexed Functions
    Indexed(..)
  , Indexable
  , Index(..)
  , (<.>), (<.), (.>)
  , icompose
  , reindex
  , indexed
  ) where

import Control.Lens.Internal

infixr 9 <.>, <., .>

-- | Permit overloading of function application for things that also admit a notion of a key or index.

-- | Provides overloading for 'Indexed' functions.
class Indexed i k where
  -- | Build a function from an 'Indexed' function
  index :: ((i -> a) -> b) -> k a b

-- | Type alias for passing around polymorphic 'Indexed' functions that can be called 'withIndex' or
-- directly as a function
type Indexable i a b = forall k. Indexed i k => k a b

instance Indexed i (->) where
  index f = f . const
  {-# INLINE index #-}

-- | A function with access to a index. This constructor may be useful when you need to store
-- a 'Indexable' in a container to avoid @ImpredicativeTypes@.
newtype Index i a b = Index { withIndex :: (i -> a) -> b }

-- | Using an equality witness to avoid potential overlapping instances
-- and aid dispatch.
instance i ~ j => Indexed i (Index j) where
  index = Index
  {-# INLINE index #-}

-- | Compose an 'Indexed' function with a non-indexed function.
--
-- Mnemonically, the @<@ points to the index we want to preserve.
(<.)  :: Indexed i k => Index i b c -> (a -> b) -> k a c
Index ibc <. ab = index $ \ia -> ibc (ab . ia)
{-# INLINE (<.) #-}

-- | Compose a non-indexed function with an 'Indexed' function.
--
-- Mnemonically, the @>@ points to the index we want to preserve.
(.>)  :: Indexed i k => (b -> c) -> Index i a b -> k a c
bc .> Index iab = index (bc . iab)
{-# INLINE (.>) #-}

-- | Remap the index.
reindex :: Indexed j k => (i -> j) -> Index i a b -> k a b
reindex ij (Index iab) = index $ \ ja -> iab $ \i -> ja (ij i)
{-# INLINE reindex #-}

-- | Composition of 'Indexed' functions
--
-- Mnemonically, the @\<@ and @\>@ points to the fact that we want to preserve the indices.
(<.>) :: Indexed (i, j) k => Index i b c -> Index j a b -> k a c
f <.> g = icompose (,) f g
{-# INLINE (<.>) #-}

-- | Composition of 'Indexed' functions with a user supplied function for combining indexs
icompose :: Indexed k r => (i -> j -> k) -> Index i b c -> Index j a b -> r a c
icompose ijk (Index ibc) (Index jab) = index $ \ka -> ibc $ \i -> jab $ \j -> ka (ijk i j)
{-# INLINE icompose #-}

-- | Transform an Traversal into an IndexedTraversal, a Fold into an IndexedFold, etc.
--
-- @
-- 'indexed' :: 'Control.Lens.Traversal.Traversal' s t a b -> 'Control.Lens.IndexedTraversal.IndexedTraversal' 'Int' s t a b
-- 'indexed' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.IndexedLens.IndexedLens' 'Int' s t a b
-- 'indexed' :: 'Control.Lens.Fold.Fold' s t          -> 'Control.Lens.IndexedFold.IndexedFold' 'Int' s t
-- 'indexed' :: 'Control.Lens.Iso.Iso' s t a b       -> 'Control.Lens.IndexedLens.IndexedLens' 'Int' s t a b
-- 'indexed' :: 'Control.Lens.Getter.Getter' s t        -> 'Control.Lens.IndexedGetter.IndexedGetter' 'Int' s t a b
-- @
indexed :: Indexed Int k => ((a -> Indexing f b) -> s -> Indexing f t) -> k (a -> f b) (s -> f t)
indexed l = index $ \iafb s -> case runIndexing (l (\a -> Indexing (\i -> IndexingResult (iafb i a) (i + 1))) s) 0 of
  IndexingResult r _ -> r
{-# INLINE indexed #-}
