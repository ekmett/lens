{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Jacket
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides internal types and functions used in the implementation
-- of @Control.Lens.Zipper@. You shouldn't need to import it directly, and the
-- exported types can be used to break 'Zipper' invariants.
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Jacket
  (
  -- * Jackets
    Jacket(..)
  , jackl, jackr
  -- * Tailoring
  , Tailor(..)
  -- * Path
  , Path(..)
  , recompress
  , startl, startr
  , movel, mover
  ) where

import Control.Applicative
import Control.Arrow
import Control.Lens.Combinators
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Data.Foldable
import Data.Monoid
import Data.Traversable

{-# ANN module "HLint: ignore Use foldl" #-}

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Char

------------------------------------------------------------------------------
-- Jacket
------------------------------------------------------------------------------

data Jacket i t b a where
  JacketAp   :: Int        -- overall size
             -> Int        -- prefix-sum including left branch
             -> Either i y -- scan left to right for leftmost i
             -> Either i y -- scan right to left for rightmost i
             -> Jacket i (x -> y) b a
             -> Jacket i x b a
             -> Jacket i y b a
  JacketPure :: x -> Jacket i x b a
  JacketLeaf :: Int -> i -> a -> Jacket i b b a

instance Functor (Jacket i t b) where
  fmap f (JacketAp s o m n x y) = JacketAp s o m n (fmap f x) (fmap f y)
  fmap _ (JacketPure x)         = JacketPure x
  fmap f (JacketLeaf o i a)     = JacketLeaf o i (f a)

instance Foldable (Jacket i t b) where
  foldMap f (JacketAp _ _ _ _ x y) = foldMap f x `mappend` foldMap f y
  foldMap _ JacketPure{}           = mempty
  foldMap f (JacketLeaf _ _ a)     = f a

instance Traversable (Jacket i t b) where
  traverse f (JacketAp s o m n x y) = JacketAp s o m n <$> traverse f x <*> traverse f y
  traverse _ (JacketPure x) = pure (JacketPure x)
  traverse f (JacketLeaf o i a) = JacketLeaf o i <$> f a

instance (Show i, Show a) => Show (Jacket i t b a) where
  showsPrec d (JacketAp _ _ _ _ x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (JacketPure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (JacketLeaf _ i a) = showParen (d > 10) $
    showString "leaf " . showsPrec 11 i . showChar ' ' . showsPrec 11 a

-- | Extract 'Either' the 'Leftmost' key or proof that no leaves exist by collapsing the 'Jacket'.
jackl :: Jacket i x b a -> Either i x
jackl (JacketAp _ _ nl _ _ _) = nl
jackl (JacketPure x)          = Right x
jackl (JacketLeaf _ i _)      = Left i
{-# INLINE jackl #-}

-- | Extract 'Either' the 'Rightmost' key or proof that no leaves exist by collapsing the 'Jacket'.
jackr :: Jacket i x b a -> Either i x
jackr (JacketAp _ _ _ nr _ _) = nr
jackr (JacketPure x)          = Right x
jackr (JacketLeaf _ i _)      = Left i
{-# INLINE jackr #-}

------------------------------------------------------------------------------
-- Tailor
------------------------------------------------------------------------------

data Tailor i a b t = Tailor Int (Int -> Jacket i t b a)

instance Functor (Tailor i a b) where
  fmap f (Tailor w k) = Tailor w $ \o -> let ko = k o in JacketAp w o (f <$> jackl ko) (f <$> jackr ko) (JacketPure f) ko
  {-# INLINE fmap #-}

instance Applicative (Tailor i a b) where
  pure a = Tailor 0 $ \_ -> JacketPure a
  {-# INLINE pure #-}
  Tailor wf mf <*> Tailor wa ma = Tailor wfa $ \o -> let
      p = o + wf
      mfo = mf o
      mao = ma p
    in JacketAp wfa p (jackl mfo <*> jackl mao) ((&) <$> jackr mao <*> jackr mfo) mfo mao
    where wfa = wf + wa
  {-# INLINE (<*>) #-}

instance (p ~ Indexed i) => Sellable p (Tailor i) where
  sell = Indexed $ \i a -> Tailor 1 $ \ o -> JacketLeaf o i a
  {-# INLINE sell #-}

instance Indexable i p => Bizarre p (Tailor i) where
  bazaar (pafb :: p a (f b)) (Tailor _ k) = go (k 0) where
    go :: Applicative f => Jacket i t b a -> f t
    go (JacketAp _ _ _ _ x y) = go x <*> go y
    go (JacketPure x)         = pure x
    go (JacketLeaf _ i a)     = indexed pafb i a
  {-# INLINE bazaar #-}

instance IndexedFunctor (Tailor i) where
  ifmap f (Tailor w k) = Tailor w $ \o -> let ko = k o in JacketAp w o (f <$> jackl ko) (f <$> jackr ko) (JacketPure f) ko
  {-# INLINE ifmap #-}

{-
instance IndexedComonad (Tailor i) where
  iextract (Tailor _ k) = go (k 0) where
    go :: Jacket i t a a -> t
    go (JacketAp _ _ _ _ x y) = go x (go y)
    go (JacketPure x)         = x
    go (JacketLeaf _ _ a)     = a
  {-# INLINE iextract #-}

  iduplicate (Tailor w k) = Tailor w (go . k) where
    go :: Jacket i t b a -> Jacket i (Tailor i x b t) x a
    go (JacketPure x)     = JacketPure (pure x)
    ...

instance a ~ b => Comonad (Tailor i a b) where
  extract   = iextract
  extend    = iextend
  duplicate = iduplicate
-}

------------------------------------------------------------------------------
-- Paths
------------------------------------------------------------------------------

-- | A Path into a 'Jacket' that ends at a 'JacketLeaf'
data Path :: * -> * -> * -> * -> * -> * where
  ApL :: Int -> Int -> !(Path i t y b a) -> !(Jacket i x b a) -> Path i t (x -> y) b a
  ApR :: Int -> Int -> !(Jacket i (x -> y) b a) -> !(Path i t y b a) -> Path i t x b a
  Start :: Path i t t b a

instance (Show i, Show a) => Show (Path i t y b a) where
  showsPrec d (ApL _ _ l r) = showParen (d > 4) $
    showsPrec 4 l . showString " <*> " . showsPrec 5 r
  showsPrec d (ApR _ _ l r) = showParen (d > 4) $
    showsPrec 4 l . showString " <*> " . showsPrec 5 r
  showsPrec _ Start = showChar '_'

instance Functor (Path i t y b) where
  fmap f (ApL s o p q) = ApL s o (fmap f p) (fmap f q)
  fmap f (ApR s o p q) = ApR s o (fmap f p) (fmap f q)
  fmap _ Start         = Start
  {-# INLINE fmap #-}

-- * Recursion
--
-- For several operations, we unroll the first step of the recursion (or part
-- of it) so GHC can inline better. There are two specific cases that we care
-- about: The "lens case", where the entire tree is just (Leaf (Identity x)), and the
-- "list case", where the traversal tree is right-biased, as in (Ap (Leaf (Identity x))
-- (Ap (Leaf (Identity y)) ...)). It should be safe to delete any of these cases.

-- | Reconstruct a 'Jacket' from a 'Path' to a leaf and a new value for the leaf.
-- recompress :: Path i z t b a -> Int -> i -> a -> Jacket i z b a

recompress :: Path i t b b a -> Int -> i -> a -> Jacket i t b a
recompress Start              o i a = JacketLeaf o i a -- Unrolled: The lens case.
recompress (ApL s oa Start r) o i a = JacketAp s oa (Left i) (Left ((id ||| const i) $ jackr r)) (JacketLeaf o i a) r -- Unrolled: The list case. In particular, a right-biased tree that we haven't moved rightward in.
recompress p o0 i a = go p (JacketLeaf o0 i a) where
  go :: Path i t x b a -> Jacket i x b a -> Jacket i t b a
  go Start          q = q
  go (ApL s oa q r) l = go q (jack s oa l r)
  go (ApR s oa l q) r = go q (jack s oa l r)
{-# INLINE recompress #-}

-- reassemble a jacket. Note: upon reassembly, both 'jackl' and 'jackr' will both be 'Right', so we could improve this a lot
jack :: Int -> Int -> Jacket i (x -> y) b a -> Jacket i x b a -> Jacket i y b a
jack s o l r = JacketAp s o (Right (fromRight (jackl l <*> jackl r))) (Right (fromRight ((&) <$> jackr r <*> jackr l))) l r
{-# INLINE jack #-}

fromRight :: Either a b -> b
fromRight (Left _)  = error "zipper: panic: missing element"
fromRight (Right b) = b
{-# INLINE fromRight #-}

startl :: forall a b i r x y. Path i y x b a -> Jacket i x b a -> r -> (Path i y b b a -> Int -> i -> a -> r) -> r
startl p0 (JacketLeaf o i a) _ kp = kp p0 o i a -- Unrolled: The lens case.
startl p0 (JacketAp s oa _ _ (JacketLeaf o i a) r) _ kp = kp (ApL s oa p0 r) o i a -- Unrolled: The list case. (Is this one a good idea?)
startl p0 c0 kn kp = go p0 c0 where
  go :: Path i y x' b a -> Jacket i x' b a -> r
  go p (JacketAp s oa _ _ l r) = case jackl l of
    Right xy -> go (ApR s oa (JacketPure xy) p) r -- skip the empty side, and record it as pure
    Left _   -> go (ApL s oa p r) l
  go p (JacketLeaf o i a) = kp p o i a
  go _ (JacketPure _) = kn
{-# INLINE startl #-}

startr :: forall a b i r x y. Path i y x b a -> Jacket i x b a -> r -> (Path i y b b a -> Int -> i -> a -> r) -> r
startr p0 (JacketLeaf o i a) _ kp = kp p0 o i a
startr p0 c0 kn kp = go p0 c0 where
  go :: Path i y x' b a -> Jacket i x' b a -> r
  go p (JacketAp s oa _ _ l r) = case jackr r of
    Right x -> go (ApL s oa p (JacketPure x)) l
    Left _  -> go (ApR s oa l p) r
  go p (JacketLeaf o i a) = kp p o i a
  go _ (JacketPure _) = kn
{-# INLINE startr #-}

-- | Move left one leaf
movel :: forall a b i r x y. Path i y x b a -> Jacket i x b a -> r -> (Path i y b b a -> Int -> i -> a -> r) -> r
movel p0 c0 kn kp = go p0 c0 where
  go :: Path i y x' b a -> Jacket i x' b a -> r
  go Start _ = kn
  go (ApR s oa l q) r = case jackr l of
    Right xy -> go q (jack s oa (JacketPure xy) r) -- l is pure
    Left _   -> startr (ApL s oa q r) l kn kp      -- l has a rightmost element, descend into it
  go (ApL s oa p r) l = go p (jack s oa l r)
{-# INLINE movel #-}

-- | Move right one leaf
mover :: forall a b i r x y. Path i y x b a -> Jacket i x b a -> r -> (Path i y b b a -> Int -> i -> a -> r) -> r
mover p0 c0 kn kp = go p0 c0 where
  go :: Path i y x' b a -> Jacket i x' b a -> r
  go Start _ = kn
  go (ApL s oa p r) l = case jackl r of
    Right x -> go p (jack s oa l (JacketPure x))
    Left _  -> startl (ApR s oa l p) r kn kp
  go (ApR s oa l q) r = go q (jack s oa l r)
{-# INLINE mover #-}
