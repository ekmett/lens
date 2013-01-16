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
  -- * TakingWhile
    TakingWhile(..)
  , runTakingWhile
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Magma
import Data.Foldable
import Data.Monoid
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Data.Traversable
import Prelude hiding ((.),id)

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
  fmap f (TakingWhile w t k) = TakingWhile w (f t) (MagmaFmap f . k)
  {-# INLINE fmap #-}

instance a ~ b => Applicative (TakingWhile p f a b) where
  pure a = TakingWhile True a $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  TakingWhile wf tf mf <*> TakingWhile wa ta ma = TakingWhile (wf && wa) (tf ta) $ \o -> case o of
    False -> MagmaPure (tf ta)
    True  -> MagmaAp (mf True) (ma wf)
  {-# INLINE (<*>) #-}

instance Corepresentable p => Bizarre p (TakingWhile p g) where
  bazaar (pafb :: p a (f b)) (TakingWhile _ _ k) = go (k True) where
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
