{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(TRUSTWORTHY) && !defined(SAFE)
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Combinators
-- Copyright   :  (C) 2012 Edward Kmett, Shachaf Ben-Kiki, Elliott Hird
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types, KindSignatures
--
-- This module is not exported from this package.
--
-- These combinators are used to reduce eta-expansion in the resulting code
-- which could otherwise cause both a constant and asymptotic slowdown to
-- code execution.
--
-- Many micro-benchmarks are improved up to 50%, and larger benchmarks can
-- win asymptotically.
----------------------------------------------------------------------------
module Control.Lens.Internal.Combinators
  ( Compose(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Internal
import Data.Monoid
#ifndef SAFE
import Unsafe.Coerce
#endif

infixr 8 #

class Compose a b where
  (#) :: (a -> b) -> (c -> a) -> c -> b
  f # g = f `seq` g `seq` \x -> f (g x)
  {-# INLINE (#) #-}

#ifndef SAFE
#define COMPOSE(a, b, f, g) \
  instance Compose (a) (b) where { \
    _ # h = unsafeCoerce h; \
    {-# INLINE (#) #-}; \
  }; \
  instance Compose (b) (a) where { \
    _ # h = unsafeCoerce h; \
    {-# INLINE (#) #-}; \
  }
#else
#define COMPOSE(a, b, f, g) \
  instance Compose (a) (b) where { \
    _ # h = h `seq` \x -> (f) (h x); \
    {-# INLINE (#) #-}; \
  } \
  instance Compose (b) (a) where { \
    _ # h = h `seq` \x -> (g) (h x); \
    {-# INLINE (#) #-}; \
  }
#endif

COMPOSE(Const r a, r, Const, getConst)
COMPOSE(ZipList a, [a], ZipList, getZipList)
COMPOSE(WrappedMonad m a, m a, WrapMonad, unwrapMonad)
COMPOSE(Last a, Maybe a, Last, getLast)
COMPOSE(First a, Maybe a, First, getFirst)
COMPOSE(Product a, a, Product, getProduct)
COMPOSE(Sum a, a, Sum, getSum)
COMPOSE(Any, Bool, Any, getAny)
COMPOSE(All, Bool, All, getAll)
COMPOSE(Dual a, a, Dual, getDual)
COMPOSE(Endo a, a -> a, Endo, appEndo)
COMPOSE(May a, Maybe a, May, getMay)
COMPOSE(Folding f a, f a, Folding, getFolding)
COMPOSE(Effect m r a, m r, Effect, getEffect)
COMPOSE(EffectRWS w st m s a, st -> m (s, st, w), EffectRWS, getEffectRWS)
COMPOSE(Accessor r a, r, Accessor, runAccessor)
COMPOSE(Err e a, Either e a, Err, getErr)
COMPOSE(Traversed f, f (), Traversed, getTraversed)
COMPOSE(Sequenced f, f (), Sequenced, getSequenced)
COMPOSE(Focusing m s a, m (s, a), Focusing, unfocusing)
COMPOSE(FocusingWith w m s a, m (s, a, w), FocusingWith, unfocusingWith)
COMPOSE(FocusingPlus w k s a, k (s, w) a, FocusingPlus, unfocusingPlus)
COMPOSE(FocusingOn f k s a, k (f s) a, FocusingOn, unfocusingOn)
COMPOSE(FocusingMay k s a, k (May s) a, FocusingMay, unfocusingMay)
COMPOSE(FocusingErr e k s a, k (Err e s) a, FocusingErr, unfocusingErr)
COMPOSE(Mutator a, a, Mutator, runMutator)
COMPOSE(Backwards f a, f a, Backwards, forwards)
