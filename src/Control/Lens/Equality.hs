{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Equality
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Control.Lens.Equality
  (
  -- * Type Equality
    Equality, Equality'
  , AnEquality, AnEquality'
  , runEq
  , substEq
  , mapEq
  , fromEq
  , simply
  -- * The Trivial Equality
  , simple
  -- * Implementation Details
  , Identical(..)
  ) where

import Control.Lens.Type
import Data.Proxy (Proxy)

#ifdef HLINT
{-# ANN module "HLint: ignore Use id" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}
#endif

-- $setup
-- >>> import Control.Lens

-----------------------------------------------------------------------------
-- Equality
-----------------------------------------------------------------------------

-- | Provides witness that @(s ~ a, b ~ t)@ holds.
data Identical a b s t where
  Identical :: Identical a b a b

-- | When you see this as an argument to a function, it expects an 'Equality'.
#if __GLASGOW_HASKELL__ >= 706
type AnEquality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = Identical a (Proxy b) a (Proxy b) -> Identical a (Proxy b) s (Proxy t)
#else
type AnEquality s t a b = Identical a (Proxy b) a (Proxy b) -> Identical a (Proxy b) s (Proxy t)
#endif

-- | A 'Simple' 'AnEquality'.
type AnEquality' s a = AnEquality s s a a

-- | Extract a witness of type 'Equality'.
runEq :: AnEquality s t a b -> Identical s t a b
runEq l = case l Identical of Identical -> Identical
{-# INLINE runEq #-}

-- | Substituting types with 'Equality'.
substEq :: AnEquality s t a b -> ((s ~ a, t ~ b) => r) -> r
substEq l = case runEq l of
  Identical -> \r -> r
{-# INLINE substEq #-}

-- | We can use 'Equality' to do substitution into anything.
#if __GLASGOW_HASKELL__ >= 706
mapEq :: forall (s :: k1) (t :: k2) (a :: k1) (b :: k2) (f :: k1 -> *) . AnEquality s t a b -> f s -> f a
#else
mapEq :: AnEquality s t a b -> f s -> f a
#endif
mapEq l r = substEq l r
{-# INLINE mapEq #-}

-- | 'Equality' is symmetric.
fromEq :: AnEquality s t a b -> Equality b a t s
fromEq l = substEq l id
{-# INLINE fromEq #-}

-- | This is an adverb that can be used to modify many other 'Lens' combinators to make them require
-- simple lenses, simple traversals, simple prisms or simple isos as input.
simply :: (Optic' p f s a -> r) -> Optic' p f s a -> r
simply = id
{-# INLINE simply #-}

-- | Composition with this isomorphism is occasionally useful when your 'Lens',
-- 'Control.Lens.Traversal.Traversal' or 'Iso' has a constraint on an unused
-- argument to force that argument to agree with the
-- type of a used argument and avoid @ScopedTypeVariables@ or other ugliness.
simple :: Equality' a a
simple = id
{-# INLINE simple #-}
