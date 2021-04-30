{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Magma
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Magma
  (
  -- * Magma
    Magma(..)
  , runMagma
  -- * Molten
  , Molten(..)
  -- * Mafic
  , Mafic(..)
  , runMafic
  -- * TakingWhile
  , TakingWhile(..)
  , runTakingWhile
  ) where

import Prelude ()

import Control.Comonad
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Prelude
import Data.Functor.Apply
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Kind
import Data.Traversable.WithIndex

------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

-- | This provides a way to peek at the internal structure of a
-- 'Control.Lens.Traversal.Traversal' or 'Control.Lens.Traversal.IndexedTraversal'
data Magma i t b a where
  MagmaAp   :: Magma i (x -> y) b a -> Magma i x b a -> Magma i y b a
  MagmaPure :: x -> Magma i x b a
  MagmaFmap :: (x -> y) -> Magma i x b a -> Magma i y b a
  Magma :: i -> a -> Magma i b b a

-- note the 3rd argument infers as phantom, but that would be unsound
type role Magma representational nominal nominal nominal

instance Functor (Magma i t b) where
  fmap f (MagmaAp x y)    = MagmaAp (fmap f x) (fmap f y)
  fmap _ (MagmaPure x)    = MagmaPure x
  fmap f (MagmaFmap xy x) = MagmaFmap xy (fmap f x)
  fmap f (Magma i a)  = Magma i (f a)

instance Foldable (Magma i t b) where
  foldMap f (MagmaAp x y)   = foldMap f x `mappend` foldMap f y
  foldMap _ MagmaPure{}     = mempty
  foldMap f (MagmaFmap _ x) = foldMap f x
  foldMap f (Magma _ a) = f a

instance Traversable (Magma i t b) where
  traverse f (MagmaAp x y)    = MagmaAp <$> traverse f x <*> traverse f y
  traverse _ (MagmaPure x)    = pure (MagmaPure x)
  traverse f (MagmaFmap xy x) = MagmaFmap xy <$> traverse f x
  traverse f (Magma i a)  = Magma i <$> f a

instance FunctorWithIndex i (Magma i t b) where
  imap f (MagmaAp x y)    = MagmaAp (imap f x) (imap f y)
  imap _ (MagmaPure x)    = MagmaPure x
  imap f (MagmaFmap xy x) = MagmaFmap xy (imap f x)
  imap f (Magma i a)      = Magma i (f i a)
  {-# INLINE imap #-}

instance FoldableWithIndex i (Magma i t b) where
  ifoldMap f (MagmaAp x y)   = ifoldMap f x `mappend` ifoldMap f y
  ifoldMap _ MagmaPure{}     = mempty
  ifoldMap f (MagmaFmap _ x) = ifoldMap f x
  ifoldMap f (Magma i a)     = f i a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i (Magma i t b) where
  itraverse f (MagmaAp x y)    = MagmaAp <$> itraverse f x <*> itraverse f y
  itraverse _ (MagmaPure x)    = pure (MagmaPure x)
  itraverse f (MagmaFmap xy x) = MagmaFmap xy <$> itraverse f x
  itraverse f (Magma i a)      = Magma i <$> f i a
  {-# INLINE itraverse #-}

instance (Show i, Show a) => Show (Magma i t b a) where
  showsPrec d (MagmaAp x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (MagmaPure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (MagmaFmap _ x) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 x
  showsPrec d (Magma i a) = showParen (d > 10) $
    showString "Magma " . showsPrec 11 i . showChar ' ' . showsPrec 11 a

-- | Run a 'Magma' where all the individual leaves have been converted to the
-- expected type
runMagma :: Magma i t a a -> t
runMagma (MagmaAp l r)   = runMagma l (runMagma r)
runMagma (MagmaFmap f r) = f (runMagma r)
runMagma (MagmaPure x)   = x
runMagma (Magma _ a) = a

------------------------------------------------------------------------------
-- Molten
------------------------------------------------------------------------------

-- | This is a a non-reassociating initially encoded version of 'Bazaar'.
newtype Molten i a b t = Molten { runMolten :: Magma i t b a }

instance Functor (Molten i a b) where
  fmap f (Molten xs) = Molten (MagmaFmap f xs)
  {-# INLINE fmap #-}

instance Apply (Molten i a b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

instance Applicative (Molten i a b) where
  pure  = Molten #. MagmaPure
  {-# INLINE pure #-}
  Molten xs <*> Molten ys = Molten (MagmaAp xs ys)
  {-# INLINE (<*>) #-}

instance Sellable (Indexed i) (Molten i) where
  sell = Indexed (\i -> Molten #. Magma i)
  {-# INLINE sell #-}

instance Bizarre (Indexed i) (Molten i) where
  bazaar f (Molten (MagmaAp x y))   = bazaar f (Molten x) <*> bazaar f (Molten y)
  bazaar f (Molten (MagmaFmap g x)) = g <$> bazaar f (Molten x)
  bazaar _ (Molten (MagmaPure x))   = pure x
  bazaar f (Molten (Magma i a)) = indexed f i a

instance IndexedFunctor (Molten i) where
  ifmap f (Molten xs) = Molten (MagmaFmap f xs)
  {-# INLINE ifmap #-}

instance IndexedComonad (Molten i) where
  iextract (Molten (MagmaAp x y))   = iextract (Molten x) (iextract (Molten y))
  iextract (Molten (MagmaFmap f y)) = f (iextract (Molten y))
  iextract (Molten (MagmaPure x))   = x
  iextract (Molten (Magma _ a)) = a

  iduplicate (Molten (Magma i a)) = Molten #. Magma i <$> Molten (Magma i a)
  iduplicate (Molten (MagmaPure x))   = pure (pure x)
  iduplicate (Molten (MagmaFmap f y)) = iextend (fmap f) (Molten y)
  iduplicate (Molten (MagmaAp x y))   = iextend (<*>) (Molten x) <*> iduplicate (Molten y)

  iextend k (Molten (Magma i a)) = (k .# Molten) . Magma i <$> Molten (Magma i a)
  iextend k (Molten (MagmaPure x))   = pure (k (pure x))
  iextend k (Molten (MagmaFmap f y)) = iextend (k . fmap f) (Molten y)
  iextend k (Molten (MagmaAp x y))   = iextend (\x' y' -> k $ x' <*> y') (Molten x) <*> iduplicate (Molten y)

instance a ~ b => Comonad (Molten i a b) where
  extract   = iextract
  {-# INLINE extract #-}
  extend    = iextend
  {-# INLINE extend #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

------------------------------------------------------------------------------
-- Mafic
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations in sums where possible.
data Mafic a b t = Mafic Int (Int -> Magma Int t b a)

-- | Generate a 'Magma' using from a prefix sum.
runMafic :: Mafic a b t -> Magma Int t b a
runMafic (Mafic _ k) = k 0

instance Functor (Mafic a b) where
  fmap f (Mafic w k) = Mafic w (MagmaFmap f . k)
  {-# INLINE fmap #-}

instance Apply (Mafic a b) where
  Mafic wf mf <.> ~(Mafic wa ma) = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<.>) #-}

instance Applicative (Mafic a b) where
  pure a = Mafic 0 $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  Mafic wf mf <*> ~(Mafic wa ma) = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<*>) #-}

instance Sellable (->) Mafic where
  sell a = Mafic 1 $ \ i -> Magma i a
  {-# INLINE sell #-}

instance Bizarre (Indexed Int) Mafic where
  bazaar (pafb :: Indexed Int a (f b)) (Mafic _ k) = go (k 0) where
    go :: Magma Int t b a -> f t
    go (MagmaAp x y)   = go x <*> go y
    go (MagmaFmap f x) = f <$> go x
    go (MagmaPure x)   = pure x
    go (Magma i a) = indexed pafb (i :: Int) a
  {-# INLINE bazaar #-}

instance IndexedFunctor Mafic where
  ifmap f (Mafic w k) = Mafic w (MagmaFmap f . k)
  {-# INLINE ifmap #-}

------------------------------------------------------------------------------
-- TakingWhile
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations where possible.
--
-- In @'TakingWhile' p g a b t@, @g@ has a @nominal@ role to avoid exposing an illegal _|_ via 'Contravariant',
-- while the remaining arguments are degraded to a @nominal@ role by the invariants of 'Magma'
data TakingWhile p (g :: Type -> Type) a b t = TakingWhile Bool t (Bool -> Magma () t b (Corep p a))
type role TakingWhile nominal nominal nominal nominal nominal

-- | Generate a 'Magma' with leaves only while the predicate holds from left to right.
runTakingWhile :: TakingWhile p f a b t -> Magma () t b (Corep p a)
runTakingWhile (TakingWhile _ _ k) = k True

instance Functor (TakingWhile p f a b) where
  fmap f (TakingWhile w t k) = let ft = f t in TakingWhile w ft $ \b -> if b then MagmaFmap f (k b) else MagmaPure ft
  {-# INLINE fmap #-}

instance Apply (TakingWhile p f a b) where
  TakingWhile wf tf mf <.> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<.>) #-}

instance Applicative (TakingWhile p f a b) where
  pure a = TakingWhile True a $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  TakingWhile wf tf mf <*> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<*>) #-}

instance Corepresentable p => Bizarre p (TakingWhile p g) where
  bazaar (pafb :: p a (f b)) ~(TakingWhile _ _ k) = go (k True) where
    go :: Magma () t b (Corep p a) -> f t
    go (MagmaAp x y)  = go x <*> go y
    go (MagmaFmap f x)  = f <$> go x
    go (MagmaPure x)    = pure x
    go (Magma _ wa) = cosieve pafb wa
  {-# INLINE bazaar #-}

-- This constraint is unused intentionally, it protects TakingWhile
instance Contravariant f => Contravariant (TakingWhile p f a b) where
  contramap _ = (<$) (error "contramap: TakingWhile")
  {-# INLINE contramap #-}

instance IndexedFunctor (TakingWhile p f) where
  ifmap = fmap
  {-# INLINE ifmap #-}
