{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
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
  , ineffective
  -- * Setters
  , Settable(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Isomorphic
import Control.Lens.Unsafe
import Control.Monad (liftM)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid
import Unsafe.Coerce

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
-- Due to the structure of this library, if you built an illegal 'Gettable'
-- instance that defined @'coerce' = 'undefined'@, it would be possible to
-- produce code that would 'unsafeCoerce'.
--
-- This would violate the promises of @SafeHaskell@.
--
-- That said, the existing instances are all safe. To verify that any
-- additional instances that *you* provide are safe, you must
--
-- > import Control.Lens.Unsafe
--
-- and provide an instance of @Trustworthy@ for your data type. That module
-- does not make @SafeHaskell@ guarantees, so by doing so you've taken the
-- @SafeHaskell@ proof obligation into your own hands.

class (Functor f, Trustworthy f) => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

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

-- | An 'Effective' 'Functor' ignores its argument and is isomorphic to a monad wrapped around a value.
--
-- That said, the monad is possibly rather unrelated to any 'Applicative' structure.
class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: Isomorphic k => k (m r) (f a)

-- | A convenient antonym that is used internally.
ineffective :: Effective m r f => Isomorphic k => k (f a) (m r)
ineffective = from effective
{-# INLINE ineffective #-}

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = isomorphic (Backwards . effective . liftM getDual) (liftM Dual . ineffective . forwards)

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class Applicative f => Settable f where
  untainted :: f a -> a

  untainted# :: (a -> f b) -> a -> b
  untainted# f = untainted . f

  tainted# :: (a -> b) -> a -> f b
  tainted# f = pure . f

-- | so you can pass our a 'Control.Lens.Setter.Setter' into combinators from other lens libraries
instance Settable Identity where
  untainted = runIdentity
  untainted# = unsafeCoerce
  {-# INLINE untainted #-}
  tainted# = unsafeCoerce
  {-# INLINE tainted# #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untainted . forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untainted . untainted . getCompose
  {-# INLINE untainted #-}
