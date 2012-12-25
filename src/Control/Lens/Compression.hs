{-# LANGUAGE PatternGuards #-}
module Control.Lens.Compression
  ( Compressed(..)
  , size
  , compressedIns
  , compressedOuts
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

-- | A path-compressed traversal.
data Compressed a
  = Ap Size !Depth (Compressed a) (Compressed a) -- size, depth, left, right
  | Leaf a
  | Pure
  deriving Show

size :: Compressed a -> Int
size (Ap s _ _ _) = s
size Leaf{}       = 1
size Pure         = 0

instance Functor Compressed where
  fmap f (Ap m n l r) = Ap m n (fmap f l) (fmap f r)
  fmap f (Leaf a)     = Leaf (f a)
  fmap _ Pure         = Pure

instance Foldable Compressed where
  foldMap f (Ap _ _ l r) = foldMap f l `mappend` foldMap f r
  foldMap f (Leaf a)     = f a
  foldMap _ Pure         = mempty

instance Traversable Compressed where
  traverse f (Ap m n l r) = Ap m n <$> traverse f l <*> traverse f r
  traverse f (Leaf a)     = Leaf <$> f a
  traverse _ Pure         = pure Pure

-- | This is an illegal Monoid.
instance Monoid (Compressed a) where
  mempty = Pure

  Pure       `mappend` Pure       = Pure
  Pure       `mappend` Ap m n l r = Ap m (n + 1) l r
  Pure       `mappend` l@Leaf{}   = l
  l@Leaf{}   `mappend` Pure       = l
  Ap m n l r `mappend` Pure       = Ap m (n + 1) l r
  l          `mappend` r          = Ap (size l + size r) 0 l r

compressedIns :: Bazaar (->) a b t -> Compressed a
compressedIns = foldMapOf (flip runBazaar) Leaf

newtype Decompression e a = Decompression { runDecompression :: Depth -> Compressed e -> a }

instance Functor (Decompression e) where
  fmap f (Decompression g) = Decompression $ \ d -> f . g d

-- | This is an illegal Applicative.
instance Applicative (Decompression e) where
  pure a = Decompression $ \ _ _ -> a
  Decompression mf <*> Decompression ma = Decompression $ \ d s -> case s of
    Ap _ n l r | d == n      ->          mf 0  l (ma 0  r)
    _          | d' <- d + 1 -> d'` seq` mf d' s (ma d' s)

compressedOuts :: Bazaar (->) a b t -> Compressed b -> t
compressedOuts bz = runDecompression go 0 where
  go = runBazaar bz $ \_ -> Decompression $ \ _ t -> case t of
    Leaf x -> x
    _      -> error "compressedOuts: wrong shape"

-- | This is only a valid 'Lens' if you don't change the shape of the Compressed tree, etc.
compressed :: ATraversal s t a b -> Lens s t (Compressed a) (Compressed b)
compressed l f s = compressedOuts bz <$> f (compressedIns bz) where
  bz = l sell s
