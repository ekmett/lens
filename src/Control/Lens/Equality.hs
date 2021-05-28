{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE Trustworthy #-}

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
  , (:~:)(..)
  , runEq
  , substEq
  , mapEq
  , fromEq
  , simply
  -- * The Trivial Equality
  , simple
  -- * 'Iso'-like functions
  , equality
  , equality'
  , withEquality
  , underEquality
  , overEquality
  , fromLeibniz
  , fromLeibniz'
  , cloneEquality
  -- * Implementation Details
  , Identical(..)
  ) where

import Control.Lens.Type
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(..))
import GHC.Exts (TYPE)
import Data.Kind (Type)

-- $setup
-- >>> import Control.Lens

#include "lens-common.h"

-----------------------------------------------------------------------------
-- Equality
-----------------------------------------------------------------------------

-- | Provides witness that @(s ~ a, b ~ t)@ holds.
data Identical a b s t where
  Identical :: Identical a b a b

-- | When you see this as an argument to a function, it expects an 'Equality'.
type AnEquality s t a b = Identical a (Proxy b) a (Proxy b) -> Identical a (Proxy b) s (Proxy t)

-- | A 'Simple' 'AnEquality'.
type AnEquality' s a = AnEquality s s a a

-- | Extract a witness of type 'Equality'.
runEq :: AnEquality s t a b -> Identical s t a b
runEq l = case l Identical of Identical -> Identical
{-# INLINE runEq #-}

-- | Substituting types with 'Equality'.
substEq :: forall s t a b rep (r :: TYPE rep).
           AnEquality s t a b -> ((s ~ a, t ~ b) => r) -> r
substEq l = case runEq l of
  Identical -> \r -> r
{-# INLINE substEq #-}

-- | We can use 'Equality' to do substitution into anything.
mapEq :: forall k1 k2 (s :: k1) (t :: k2) (a :: k1) (b :: k2) (f :: k1 -> Type) . AnEquality s t a b -> f s -> f a
mapEq l r = substEq l r
{-# INLINE mapEq #-}

-- | 'Equality' is symmetric.
fromEq :: AnEquality s t a b -> Equality b a t s
fromEq l = substEq l id
{-# INLINE fromEq #-}

-- | This is an adverb that can be used to modify many other 'Lens' combinators to make them require
-- simple lenses, simple traversals, simple prisms or simple isos as input.
simply :: forall p f s a rep (r :: TYPE rep).
  (Optic' p f s a -> r) -> Optic' p f s a -> r
simply = id
{-# INLINE simply #-}

-- | Composition with this isomorphism is occasionally useful when your 'Lens',
-- 'Control.Lens.Traversal.Traversal' or 'Iso' has a constraint on an unused
-- argument to force that argument to agree with the
-- type of a used argument and avoid @ScopedTypeVariables@ or other ugliness.
simple :: Equality' a a
simple = id
{-# INLINE simple #-}

cloneEquality :: AnEquality s t a b -> Equality s t a b
cloneEquality an = substEq an id
{-# INLINE cloneEquality #-}

-- | Construct an 'Equality' from explicit equality evidence.
equality :: s :~: a -> b :~: t -> Equality s t a b
equality Refl Refl = id
{-# INLINE equality #-}

-- | A 'Simple' version of 'equality'
equality' :: a :~: b -> Equality' a b
equality' Refl = id
{-# INLINE equality' #-}

-- | Recover a "profunctor lens" form of equality. Reverses 'fromLeibniz'.
overEquality :: AnEquality s t a b -> p a b -> p s t
overEquality an = substEq an id
{-# INLINE overEquality #-}

-- | The opposite of working 'overEquality' is working 'underEquality'.
underEquality :: AnEquality s t a b -> p t s -> p b a
underEquality an = substEq an id
{-# INLINE underEquality #-}

-- | Convert a "profunctor lens" form of equality to an equality. Reverses
-- 'overEquality'.
--
-- The type should be understood as
--
-- @fromLeibniz :: (forall p. p a b -> p s t) -> Equality s t a b@
fromLeibniz :: (Identical a b a b -> Identical a b s t) -> Equality s t a b
fromLeibniz f = case f Identical of Identical -> id
{-# INLINE fromLeibniz #-}

-- | Convert Leibniz equality to equality. Reverses 'mapEq' in 'Simple' cases.
--
-- The type should be understood as
--
-- @fromLeibniz' :: (forall f. f s -> f a) -> Equality' s a@
fromLeibniz' :: (s :~: s -> s :~: a) -> Equality' s a
-- Note: even though its type signature mentions (:~:), this function works just
-- fine in base versions before 4.7.0; it just requires a polymorphic argument!
fromLeibniz' f = case f Refl of Refl -> id
{-# INLINE fromLeibniz' #-}

-- | A version of 'substEq' that provides explicit, rather than implicit,
-- equality evidence.
withEquality :: forall s t a b rep (r :: TYPE rep).
   AnEquality s t a b -> (s :~: a -> b :~: t -> r) -> r
withEquality an = substEq an (\f -> f Refl Refl)
{-# INLINE withEquality #-}
