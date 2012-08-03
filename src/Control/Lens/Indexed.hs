{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Lens.Indexed
  (
  -- * Overloading indexed functions
    Indexed(..)
  , Indexable
  , Index(..)
  , (.@)
  , composeWithIndex
  , reindex
  -- * Indexed traversals
  , IndexedTraversal
  , SimpleIndexedTraversal
  , traverseWithIndexOf
  -- * Indexed folds
  , IndexedFold
  -- * Common indexed traversals
  , traverseList
  --, indexed
  --, traverseMap
  ) where

import Control.Applicative
import Data.Monoid
-- import Data.Traversable
-- import Data.Map as Map

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
-- a 'HasIndex' 
newtype Index i a b = Index { withIndex :: (i -> a) -> b }

-- | Using an equality witness to avoid potential overlapping instances
-- and aid dispatch.
instance i ~ j => Indexed i (Index j) where
  index = Index
  {-# INLINE index #-}

-- |
reindex :: Indexed j k => (i -> j) -> Index i a b -> k a b
reindex ij (Index iab) = index $ \ ja -> iab $ \i -> ja (ij i)
{-# SPECIALIZE reindex :: (i -> j) -> Index i a b -> Index j a b #-}
{-# SPECIALIZE reindex :: (i -> j) -> Index i a b -> a -> b #-}

infixr 9 .@
-- | Composition of indexed functions
(.@) :: Indexed (i, j) k => Index i b c -> Index j a b -> k a c
f .@ g = composeWithIndex (,) f g
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> Index (i,j) a c #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> a -> c #-}

-- | Composition of indexed functions with a user supplied function for combining indexs
composeWithIndex :: Indexed k r => (i -> j -> k) -> Index i b c -> Index j a b -> r a c
composeWithIndex ijk (Index ibc) (Index jab) = index $ \ka -> ibc $ \i -> jab $ \j -> ka (ijk i j)
{-# INLINE composeWithIndex #-}
{-# SPECIALIZE composeWithIndex :: (i -> j -> k) -> Index i b c -> Index j a b -> a -> c #-}
{-# SPECIALIZE composeWithIndex :: (i -> j -> k) -> Index i b c -> Index j a b -> Index k a c #-}

-- | Every indexed traversal is a valid Traversal or indexed fold.
--
-- The Traversal laws are still required to hold. Moreover, each index should be distinct.
type IndexedTraversal i a b c d = forall f k. (Indexed i k, Applicative f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i a b = IndexedTraversal i a a b b

-- |
--
-- > traverseWithIndexOf :: IndexedTraversable i a b c d -> (i -> c -> f d) -> a -> f b
traverseWithIndexOf :: Index i (c -> f d) (a -> f b) -> (i -> c -> f d) -> a -> f b
traverseWithIndexOf = withIndex
{-# INLINE traverseWithIndexOf #-}

-- | Every indexed fold is a valid Fold
type IndexedFold i a c = forall k m b d. (Indexed i k, Monoid m) => k (c -> Const m d) (a -> Const m b)

-- foldMapWithIndexOf :: Indexing i (c -> Const m d) (a -> Const m b) -> (i -> c -> m) -> a -> m
-- foldMapWithIndexOf (Indexing f) icm a = getConst . f (\i -> Const . icm i)

traverseList :: IndexedTraversal Int [a] [b] a b
traverseList = index $ go (0::Int) where
  go n f (x:xs) = (:) <$> f n x <*> go (n + 1) f xs
  go _ _ [] = pure []
{-# INLINE traverseList #-}

-- build an indexed traversal from a traversal
-- indexed :: Indexed Int k => LensLike (Indexed f) a b c d -> k (c -> f d) (a -> f b)
-- indexedFold
