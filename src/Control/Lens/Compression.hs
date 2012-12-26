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

-- Manual evaluations some function calls to clarify what's going on:

-- Evaluation of @compressedIns (both sell (0,1))@:
--
-- @
-- compressedIns (both sell (0,1))
-- compressedIns (both (\a -> Bazaar ($ a)) (0,1))
-- compressedIns ((\f (a,b) -> (,) <$> f a <$> f b) (\a -> Bazaar ($ a)) (0,1))
-- compressedIns (Bazaar (\f -> (,) <$> f 0 <*> f 1))
-- foldMapOf (flip runBazaar) Leaf (Bazaar (\f -> (,) <$> f 0 <*> f 1))
-- getConst $ flip runBazaar (Const . Leaf) (Bazaar (\f -> (,) <$> f 0 <*> f 1))
-- getConst $ (\f -> (,) <$> f 0 <*> f 1) (Const . Leaf)
-- getConst $ (,) <$> Const (Leaf 0) <*> Const (Leaf 1)
-- Leaf 0 <> Leaf 1
-- Ap 2 0 (Leaf 0) (Leaf 1)
-- @

-- Evaluation of @compressedOuts (_1 sell (0,1)) (Leaf 2)@:
--
-- @
-- compressedOuts (_1 sell (0,1)) (Leaf 2)
-- runDecompression (runBazaar (_1 sell (0,1)) (\_ -> Decompression (\_ (Leaf x) -> x))) 0 (Leaf 2)
-- runDecompression ((\f -> _1 f (0,1)) (\_ -> Decompression (\_ (Leaf x) -> x))) 0 (Leaf 2)
-- runDecompression (_1 (\_ -> Decompression (\_ (Leaf x) -> x)) (0,1)) 0 (Leaf 2)
-- runDecompression ((\f -> (,1) <$> f 0) (\_ -> Decompression (\_ (Leaf x) -> x))) 0 (Leaf 2)
-- runDecompression ((,1) <$> Decompression (\_ (Leaf x) -> x)) 0 (Leaf 2)
-- runDecompression (Decompression (\d -> (,1) . (\_ (Leaf x) -> x) d)) 0 (Leaf 2)
-- (\d -> (,1) . (\_ (Leaf x) -> x) d) 0 (Leaf 2)
-- ((,1) . (\_ (Leaf x) -> x)) 0 (Leaf 2)
-- ((,1) . (\(Leaf x) -> x)) (Leaf 2)
-- (2,1)
-- @

-- Evaluation of @compressedOuts (both sell (0,1)) (Leaf 2 <> Leaf 3)@:
--
-- @
-- compressedOuts (both sell (0,1)) (Leaf 2 <> Leaf 3)
-- … (skipped almost identical steps to the previous one)
-- runDecompression ((,) <$> Decompression (\_ (Leaf x) -> x) <*> Decompression (\_ (Leaf x) -> x)) 0 (Leaf 2 <> Leaf 3)
-- runDecompression (Decompression (\d -> (,) . (\_ (Leaf x) -> x) d) <*> Decompression (\_ (Leaf x) -> x)) 0 (Leaf 2 <> Leaf 3)
-- runDecompression (Decompression (\d s -> case s of …)) (Leaf 2 <> Leaf 3)
-- (\d s -> case s of …) 0 (Leaf 2 <> Leaf 3)
-- (\d s -> case s of …) 0 (Ap 2 0 (Leaf 2) (Leaf 3))
-- (\s -> case s of Ap _ n l r | 0 == n -> ((,) . (\_ (Leaf x) -> x)) 0 l ((\_ (Leaf x) -> x) 0 r) | …) (Ap 2 0 (Leaf 2) (Leaf 3))
-- (\s -> case s of Ap _ n l r | 0 == n -> ((,) . (\(Leaf x) -> x)) l ((\(Leaf x) -> x) r) | …) (Ap 2 0 (Leaf 2) (Leaf 3))
-- ((,) . (\(Leaf x) -> x)) (Leaf 2) ((\(Leaf x) -> x) (Leaf 3))
-- (2,3)
-- @
