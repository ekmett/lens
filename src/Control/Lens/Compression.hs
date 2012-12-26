{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable#-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
module Control.Lens.Compression
  ( Compressed(..)
  , size
  , compressing
  , decompressing
  , compressed
  ) where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Lens
import Control.Lens.Traversal
import Data.Monoid
import Data.Foldable

type Depth = Int
type Size = Int -- computed lazily

------------------------------------------------------------------------------
-- Non-Empty, Path-Compressed Data
------------------------------------------------------------------------------

data Compressed a
  = Ap Size !Depth (Compressed a) (Compressed a) -- size, depth, left, right
  | Leaf a
  deriving Show

size :: Compressed a -> Int
size (Ap s _ _ _) = s
size Leaf{}       = 1
{-# INLINE size #-}

instance Functor Compressed where
  fmap f (Ap m n l r) = Ap m n (fmap f l) (fmap f r)
  fmap f (Leaf a)     = Leaf (f a)
  {-# INLINE fmap #-}

instance Foldable Compressed where
  foldMap f (Ap _ _ l r) = foldMap f l `mappend` foldMap f r
  foldMap f (Leaf a)     = f a
  {-# INLINE foldMap #-}

instance Traversable Compressed where
  traverse f (Ap m n l r) = Ap m n <$> traverse f l <*> traverse f r
  traverse f (Leaf a)     = Leaf <$> f a
  {-# INLINE traverse #-}

bump :: Compressed a -> Compressed a
bump (Ap m n l r) = Ap m (n + 1) l r
bump (Leaf a)     = Leaf a
{-# INLINE bump #-}

------------------------------------------------------------------------------
-- Path-Compression
------------------------------------------------------------------------------

newtype Compressing a = Compressing { runCompressing :: Maybe (Compressed a) }
  deriving (Functor, Foldable, Traversable)

-- | An illegal 'Monoid'
instance Monoid (Compressing a) where
  mempty = Compressing Nothing
  {-# INLINE mempty #-}

  Compressing Nothing  `mappend` Compressing Nothing  = Compressing Nothing
  Compressing Nothing  `mappend` Compressing (Just r) = Compressing $ Just (bump r)
  Compressing (Just l) `mappend` Compressing Nothing  = Compressing $ Just (bump l)
  Compressing (Just l) `mappend` Compressing (Just r) = Compressing $ Just $ Ap (size l + size r) 0 l r
  {-# INLINE mappend #-}

-- | Attempt to compress a 'Traversable'
compressing :: Bazaar (->) a b t -> Maybe (Compressed a)
compressing = runCompressing . foldMapOf (flip runBazaar) (Compressing . Just . Leaf)
{-# INLINE compressing #-}

------------------------------------------------------------------------------
-- Path-Decompression
------------------------------------------------------------------------------

newtype Decompressing e a = Decompressing { runDecompressing :: Depth -> Maybe (Compressed e) -> a }

instance Functor (Decompressing e) where
  fmap f (Decompressing g) = Decompressing $ \ d -> f . g d
  {-# INLINE fmap #-}

-- | This is an illegal 'Applicative'.
instance Applicative (Decompressing e) where
  pure a = Decompressing $ \ _ _ -> a
  {-# INLINE pure #-}
  Decompressing mf <*> Decompressing ma = Decompressing $ \ d s -> case s of
    Just (Ap _ n l r) | d == n      ->          mf 0  (Just l) (ma 0 (Just r))
    _                 | d' <- d + 1 -> d' `seq` mf d' s (ma d' s)
  {-# INLINE (<*>) #-}

decompressing :: Bazaar (->) a b t -> Maybe (Compressed b) -> t
decompressing bz = runDecompressing go 0 where
  go = runBazaar bz $ \_ -> Decompressing $ \ _ t -> case t of
    Just (Leaf x) -> x
    _             -> error "decompressing: wrong shape"
{-# INLINE decompressing #-}

-- | This is only a valid 'Lens' if you don't change the shape of the 'Compressed' tree.
compressed :: ATraversal s t a b -> Lens s t (Maybe (Compressed a)) (Maybe (Compressed b))
compressed l f s = decompressing bz <$> f (compressing bz) where
  bz = l sell s
{-# INLINE compressed #-}
