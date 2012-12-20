{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(TRUSTWORTHY) && !defined(SAFE)
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Classes
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Control.Lens.Classes
  (
  -- * Getters
    Gettable(..)
  , noEffect
  -- * Actions
  , Effective(..)
  -- * Setters
  , Settable(..)
  -- * Costrong
  , Costrong(..)
  -- * Algebraics
  , Algebraic(..)
  -- * Indexable
  , Indexable(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards (Backwards(..))
import Control.Category
import Control.Comonad
import Control.Monad (liftM)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Dual(..))
import Data.Profunctor
import Data.Proxy
import Prelude hiding ((.),id)
#ifndef SAFE
import Unsafe.Coerce (unsafeCoerce)
#endif

#ifndef SAFE
#define UNSAFELY(x) unsafeCoerce
#else
#define UNSAFELY(f) (\g -> g `seq` \x -> (f) (g x))
#endif

-------------------------------------------------------------------------------
-- Costrong Functors
-------------------------------------------------------------------------------

class Functor f => Costrong f where
  costrength :: f (Either a b) -> Either a (f b)

instance Costrong Identity where
  costrength = either Left (Right . Identity) . runIdentity
  {-# INLINE costrength #-}

instance Costrong (Const r) where
  costrength = Right . coerce
  {-# INLINE costrength #-}

instance Costrong Proxy where
  costrength = Right . coerce
  {-# INLINE costrength #-}

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | Generalizing 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages
--
-- A 'Gettable' 'Functor' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- To ensure this, an instance of 'Gettable' is required to satisfy:
--
-- @'id' = 'fmap' f = 'coerce'@
--
-- Which is equivalent to making a @'Gettable' f@ an \"anyvariant\" functor.
--

class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

instance Gettable Proxy where
  coerce = reproxy

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Applicative f, Gettable f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}

-------------------------------------------------------------------------------
-- Programming with Effects
-------------------------------------------------------------------------------

-- | An 'Effective' 'Functor' ignores its argument and is isomorphic to a 'Monad' wrapped around a value.
--
-- That said, the 'Monad' is possibly rather unrelated to any 'Applicative' structure.
class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: m r -> f a
  ineffective :: f a -> m r

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = Backwards . effective . liftM getDual
  ineffective = liftM Dual . ineffective . forwards

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class Applicative f => Settable f where
  untainted :: f a -> a

  untaintedDot :: (a -> f b) -> a -> b
  untaintedDot g = g `seq` \x -> untainted (g x)

  taintedDot :: (a -> b) -> a -> f b
  taintedDot g = g `seq` \x -> pure (g x)

-- | so you can pass our a 'Control.Lens.Setter.Setter' into combinators from other lens libraries
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = UNSAFELY(runIdentity)
  {-# INLINE untaintedDot #-}
  taintedDot = UNSAFELY(Identity)
  {-# INLINE taintedDot #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untainted . forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untainted . untainted . getCompose
  {-# INLINE untainted #-}

-----------------------------------------------------------------------------
-- Algebraic overloading
-----------------------------------------------------------------------------

-- | Used to provide overloading of symmetric lenses as regular lenses.
--
-- There are two relevant instances: @(->)@ (for using an 'Iso'/'Prism' as a
-- regular lens) and @'Cokleisli' g@ (for using it as a symmetric lens).
--
class Algebraic g k | k -> g where
  algebraic :: (g a -> b) -> k a b
  runAlgebraic :: k a b -> g a -> b
  mapAlgebraic :: (k a b -> k s t) -> (g a -> b) -> g s -> t
  mapAlgebraic f = runAlgebraic . f . algebraic
  {-# INLINE mapAlgebraic #-}

instance Algebraic Identity (->) where
  -- This instance should use strict composition.
  algebraic f = f . Identity
  runAlgebraic f = f . runIdentity

instance Algebraic g (Cokleisli g) where
  algebraic = Cokleisli
  runAlgebraic = runCokleisli

----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Profunctor c => Indexable i c where
  -- | Build a function from an 'Indexed' function
  indexed :: c a b -> i -> a -> b

instance Indexable i (->) where
  indexed = const
  {-# INLINE indexed #-}
