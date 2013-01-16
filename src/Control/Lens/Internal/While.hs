{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Module      :  Control.Lens.Internal.While
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.While
  (
  -- * While
    While(..)
  , runWhile
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
-- While
------------------------------------------------------------------------------

data While t b a where
  WhileAp   :: Bool -> While (x -> y) b a -> While x b a -> While y b a
  WhilePure :: t -> While t b a
  WhileFmap :: (x -> y) -> While x b a -> While y b a
  WhileLeaf :: Bool -> a -> While b b a

instance Functor (While t b) where
  fmap f (WhileAp b x y)  = WhileAp b (fmap f x) (fmap f y)
  fmap _ (WhilePure x)    = WhilePure x
  fmap f (WhileFmap xy x) = WhileFmap xy (fmap f x)
  fmap f (WhileLeaf b a) = WhileLeaf b (f a)

instance Foldable (While t b) where
  foldMap f (WhileAp b x y)  = foldMap f x `mappend` foldMap f y
  foldMap _ WhilePure{}      = mempty
  foldMap f (WhileFmap _ x)  = foldMap f x
  foldMap f (WhileLeaf _ a)  = f a

instance Traversable (While t b) where
  traverse f (WhileAp b x y)  = WhileAp b <$> traverse f x <*> traverse f y
  traverse _ (WhilePure x)    = pure (WhilePure x)
  traverse f (WhileFmap xy x) = WhileFmap xy <$> traverse f x
  traverse f (WhileLeaf b a)  = WhileLeaf b <$> f a

instance Show a => Show (While t b a) where
  showsPrec d (WhileAp _ x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (WhilePure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (WhileFmap _ x) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 x
  showsPrec d (WhileLeaf b a) = showParen (d > 10) $
    showString "while " . showsPrec 11 b . showChar ' ' . showsPrec 11 a

runWhile :: While t a a -> t
runWhile (WhileAp _ l r) = runWhile l (runWhile r)
runWhile (WhileFmap f r) = f (runWhile r)
runWhile (WhilePure x)   = x
runWhile (WhileLeaf _ a) = a

------------------------------------------------------------------------------
-- TakingWhile
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations in sums where possible.
data TakingWhile p (f :: * -> *) a b t = TakingWhile Bool (Bool -> While t b (Corep p a))

runTakingWhile :: Corepresentable p => TakingWhile p f a b t -> While t b (Corep p a)
runTakingWhile (TakingWhile _ k) = k True

instance Functor (TakingWhile p f a b) where
  fmap f (TakingWhile w k) = TakingWhile w (WhileFmap f . k)
  {-# INLINE fmap #-}

instance (a ~ b) => Applicative (TakingWhile p f a b) where
  pure a = TakingWhile True $ \_ -> WhilePure a
  {-# INLINE pure #-}
  TakingWhile wf mf <*> TakingWhile wa ma = TakingWhile (wf && wa) $ \o -> let owf = o && wf in WhileAp owf (mf o) (ma owf)
  {-# INLINE (<*>) #-}

instance Corepresentable p => Bizarre p (TakingWhile p g) where
  bazaar (pafb :: p a (f b)) (TakingWhile _ k) = go (k True) where
    go :: Applicative f => While t b (Corep p a) -> f t
    go (WhileAp _ x y)  = go x <*> go y
    go (WhileFmap f x)  = f <$> go x
    go (WhilePure x)    = pure x
    go (WhileLeaf _ wa) = corep pafb wa
  {-# INLINE bazaar #-}

instance Gettable f => Gettable (TakingWhile p f a b) where
  coerce = (<$) (error "coerced TakingWhile")

instance Corepresentable p => IndexedFunctor (TakingWhile p f) where
  ifmap f (TakingWhile w k) = TakingWhile w (WhileFmap f . k)
  {-# INLINE ifmap #-}
