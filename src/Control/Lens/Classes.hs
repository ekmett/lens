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
    Pointed(..)
  , Copointed(..)
  , Costrong(..)
  , costrengthGettable
  , costrengthSettable
  -- * Getters
  , Gettable(..)
  , noEffect
  -- * Actions
  , Effective(..)
  -- * Setters
  , Settable(..)
  -- * Indexable
  , Indexable(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards (Backwards(..))
import Control.Monad (liftM)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Dual(..), Monoid(..))
import Data.Proxy
#ifndef SAFE
import Unsafe.Coerce (unsafeCoerce)
#endif

class Pointed f where
  point :: a -> f a

instance Pointed Identity where
  point = Identity
  {-# INLINE point #-}

instance Pointed Proxy where
  point _ = Proxy
  {-# INLINE point #-}

-- FIXME: should be Default r, technically
instance Monoid r => Pointed (Const r) where
  point _ = Const mempty
  {-# INLINE point #-}

instance Pointed f => Pointed (Backwards f) where
  point = Backwards . point
  {-# INLINE point #-}

instance (Pointed f, Pointed g) => Pointed (Compose f g) where
  point = Compose . point . point
  {-# INLINE point #-}

class Copointed f where
  copoint :: f a -> a

instance Copointed Identity where
  copoint = runIdentity
  {-# INLINE copoint #-}

instance Copointed f => Copointed (Backwards f) where
  copoint = copoint . forwards
  {-# INLINE copoint #-}

instance (Copointed f, Copointed g) => Copointed (Compose f g) where
  copoint = copoint . copoint . getCompose
  {-# INLINE copoint #-}

class Functor f => Costrong f where
  costrength :: f (Either a b) -> Either a (f b)

instance Costrong Identity where
  costrength = costrengthSettable
  {-# INLINE costrength #-}

instance Costrong (Const r) where
  costrength = costrengthGettable
  {-# INLINE costrength #-}

instance Costrong Proxy where
  costrength = costrengthGettable
  {-# INLINE costrength #-}

instance Costrong f => Costrong (Backwards f) where
  costrength = fmap Backwards . costrength . forwards
  {-# INLINE costrength #-}

instance (Costrong f, Costrong g) => Costrong (Compose f g) where
  costrength = fmap Compose . costrength . fmap costrength . getCompose
  {-# INLINE costrength #-}

costrengthSettable :: Settable f => f (Either a b) -> Either a (f b)
costrengthSettable = either Left (Right . point) . copoint
{-# INLINE costrengthSettable #-}

costrengthGettable :: Gettable f => f (Either a b) -> Either a (f b)
costrengthGettable = Right . coerce
{-# INLINE costrengthGettable #-}

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

class Costrong f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable Proxy where
  coerce = reproxy
  {-# INLINE coerce #-}

instance Gettable (Const r) where
  coerce (Const m) = Const m
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards
  {-# INLINE coerce #-}

instance (Costrong f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose
  {-# INLINE coerce #-}

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
class (Pointed f, Copointed f, Costrong f) => Settable f

-- | so you can pass our a 'Control.Lens.Setter.Setter' into combinators from other lens libraries
instance Settable Identity

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f)

instance (Settable f, Settable g) => Settable (Compose f g)

----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Indexable i k where
  -- | Build a function from an 'Indexed' function
  indexed :: ((i -> a) -> b) -> k a b

instance Indexable i (->) where
  indexed f = f . const
  {-# INLINE indexed #-}
