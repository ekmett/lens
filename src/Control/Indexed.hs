{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Indexed
  (
  -- * Overloading indexed functions
    Indexed(..)
  , Indexable
  , Index(..)
  , (.@)
  , composeWithIndex
  , reindex
  ) where

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

