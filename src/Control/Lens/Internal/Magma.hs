{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Magma
-- Copyright   :  (C) 2012-2013 Edward Kmett
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
  , leaf
  -- * Molten
  , Molten(..)
  -- * Mafic
  , Mafic(..)
  , runMafic
  -- * TakingWhile
  , TakingWhile(..)
  , runTakingWhile
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Data.Foldable
import Data.Monoid
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Data.Traversable
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

data Magma i t b a where
  MagmaAp   :: Magma i (x -> y) b a -> Magma i x b a -> Magma i y b a
  MagmaPure :: x -> Magma i x b a
  MagmaFmap :: (x -> y) -> Magma i x b a -> Magma i y b a
  MagmaLeaf :: i -> a -> Magma i b b a

instance Functor (Magma i t b) where
  fmap f (MagmaAp x y)    = MagmaAp (fmap f x) (fmap f y)
  fmap _ (MagmaPure x)    = MagmaPure x
  fmap f (MagmaFmap xy x) = MagmaFmap xy (fmap f x)
  fmap f (MagmaLeaf i a)  = MagmaLeaf i (f a)

instance Foldable (Magma i t b) where
  foldMap f (MagmaAp x y)   = foldMap f x `mappend` foldMap f y
  foldMap _ MagmaPure{}     = mempty
  foldMap f (MagmaFmap _ x) = foldMap f x
  foldMap f (MagmaLeaf _ a) = f a

instance Traversable (Magma i t b) where
  traverse f (MagmaAp x y)    = MagmaAp <$> traverse f x <*> traverse f y
  traverse _ (MagmaPure x)    = pure (MagmaPure x)
  traverse f (MagmaFmap xy x) = MagmaFmap xy <$> traverse f x
  traverse f (MagmaLeaf i a)  = MagmaLeaf i <$> f a

leaf :: i -> a -> Magma i b b a
leaf = MagmaLeaf

instance (Show i, Show a) => Show (Magma i t b a) where
  showsPrec d (MagmaAp x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (MagmaPure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (MagmaFmap _ x) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 x
  showsPrec d (MagmaLeaf i a) = showParen (d > 10) $
    showString "leaf " . showsPrec 11 i . showChar ' ' . showsPrec 11 a

runMagma :: Magma i t a a -> t
runMagma (MagmaAp l r)   = runMagma l (runMagma r)
runMagma (MagmaFmap f r) = f (runMagma r)
runMagma (MagmaPure x)   = x
runMagma (MagmaLeaf _ a) = a

------------------------------------------------------------------------------
-- Molten
------------------------------------------------------------------------------

-- | A non-reassociating initially encoded version of 'Bazaar'.
newtype Molten i a b t = Molten { runMolten :: Magma i t b a }

instance Functor (Molten i a b) where
  fmap f (Molten xs) = Molten (MagmaFmap f xs)

instance Applicative (Molten i a b) where
  pure  = Molten #. MagmaPure
  Molten xs <*> Molten ys = Molten (MagmaAp xs ys)

instance (p ~ Indexed i) => Sellable p (Molten i) where
  sell = Indexed (\i -> Molten #. MagmaLeaf i)

instance Bizarre (Indexed i) (Molten i) where
  bazaar f (Molten (MagmaAp x y))   = bazaar f (Molten x) <*> bazaar f (Molten y)
  bazaar f (Molten (MagmaFmap g x)) = g <$> bazaar f (Molten x)
  bazaar _ (Molten (MagmaPure x))   = pure x
  bazaar f (Molten (MagmaLeaf i a)) = indexed f i a

instance IndexedFunctor (Molten i) where
  ifmap f (Molten xs) = Molten (MagmaFmap f xs)

instance IndexedComonad (Molten i) where
  iextract (Molten (MagmaAp x y))   = iextract (Molten x) (iextract (Molten y))
  iextract (Molten (MagmaFmap f y)) = f (iextract (Molten y))
  iextract (Molten (MagmaPure x))   = x
  iextract (Molten (MagmaLeaf _ a)) = a

  iduplicate (Molten (MagmaLeaf i a)) = Molten #. MagmaLeaf i <$> Molten (MagmaLeaf i a)
  iduplicate (Molten (MagmaPure x))   = pure (pure x)
  iduplicate (Molten (MagmaFmap f y)) = iextend (fmap f) (Molten y)
  iduplicate (Molten (MagmaAp x y))   = iextend (<*>) (Molten x) <*> iduplicate (Molten y)

  iextend k (Molten (MagmaLeaf i a)) = (k .# Molten) . MagmaLeaf i <$> Molten (MagmaLeaf i a)
  iextend k (Molten (MagmaPure x))   = pure (k (pure x))
  iextend k (Molten (MagmaFmap f y)) = iextend (k . fmap f) (Molten y)
  iextend k (Molten (MagmaAp x y))   = iextend (\x' y' -> k $ x' <*> y') (Molten x) <*> iduplicate (Molten y)

instance a ~ b => Comonad (Molten i a b) where
  extract   = iextract
  extend    = iextend
  duplicate = iduplicate

------------------------------------------------------------------------------
-- Mafic
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations in sums where possible.
data Mafic a b t = Mafic Int (Int -> Magma Int t b a)

runMafic :: Mafic a b t -> Magma Int t b a
runMafic (Mafic _ k) = k 0

instance Functor (Mafic a b) where
  fmap f (Mafic w k) = Mafic w (MagmaFmap f . k)
  {-# INLINE fmap #-}

instance Applicative (Mafic a b) where
  pure a = Mafic 0 $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  Mafic wf mf <*> Mafic wa ma = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<*>) #-}

instance p ~ (->) => Sellable p Mafic where
  sell a = Mafic 1 $ \ i -> MagmaLeaf i a
  {-# INLINE sell #-}

instance Bizarre (Indexed Int) Mafic where
  bazaar (pafb :: Indexed Int a (f b)) (Mafic _ k) = go (k 0) where
    go :: Applicative f => Magma Int t b a -> f t
    go (MagmaAp x y)   = go x <*> go y
    go (MagmaFmap f x) = f <$> go x
    go (MagmaPure x)   = pure x
    go (MagmaLeaf i a) = indexed pafb (i :: Int) a
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
data TakingWhile p (g :: * -> *) a b t = TakingWhile Bool t (Bool -> Magma () t b (Corep p a))

runTakingWhile :: Corepresentable p => TakingWhile p f a b t -> Magma () t b (Corep p a)
runTakingWhile (TakingWhile _ _ k) = k True

instance Functor (TakingWhile p f a b) where
  fmap f (TakingWhile w t k) = let ft = f t in TakingWhile w ft $ \b -> if b then MagmaFmap f (k b) else MagmaPure ft
  {-# INLINE fmap #-}

instance Applicative (TakingWhile p f a b) where
  pure a = TakingWhile True a $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  TakingWhile wf tf mf <*> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<*>) #-}

instance Corepresentable p => Bizarre p (TakingWhile p g) where
  bazaar (pafb :: p a (f b)) ~(TakingWhile _ _ k) = go (k True) where
    go :: Applicative f => Magma () t b (Corep p a) -> f t
    go (MagmaAp x y)  = go x <*> go y
    go (MagmaFmap f x)  = f <$> go x
    go (MagmaPure x)    = pure x
    go (MagmaLeaf _ wa) = corep pafb wa
  {-# INLINE bazaar #-}

instance Gettable f => Gettable (TakingWhile p f a b) where
  coerce = (<$) (error "coerced TakingWhile")

instance IndexedFunctor (TakingWhile p f) where
  ifmap = fmap
  {-# INLINE ifmap #-}
