{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Getter
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Getter
  (
  -- * Internal Classes
  -- ** Getters
    Gettable(..)
  , noEffect
  , Accessor(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Data.Functor.Apply
import Data.Functor.Compose
import Data.Profunctor.Unsafe
import Data.Semigroup

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | Generalizing 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages.
--
-- A 'Gettable' 'Functor' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- To ensure this, an instance of 'Gettable' is required to satisfy:
--
-- @'id' = 'fmap' f = 'coerce'@
--
-- Which is equivalent to making a @'Gettable' f@ an \"anyvariant\"
-- 'Functor'.

class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards #. coerce .# forwards
  {-# INLINE coerce #-}

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose #. fmap coerce .# getCompose
  {-# INLINE coerce #-}

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Applicative f, Gettable f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

-- | Used instead of 'Const' to report
--
-- @No instance for ('Control.Lens.Setter.Internal.Settable' 'Accessor')@
--
-- when the user attempts to misuse a 'Control.Lens.Setter.Setter' as a
-- 'Control.Lens.Getter.Getter', rather than a monolithic unification error.
newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m
  {-# INLINE fmap #-}

instance Semigroup r => Apply (Accessor r) where
  Accessor a <.> Accessor b = Accessor (a <> b)
  {-# INLINE (<.>) #-}

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  {-# INLINE pure #-}
  Accessor a <*> Accessor b = Accessor (mappend a b)
  {-# INLINE (<*>) #-}

instance Gettable (Accessor r) where
  coerce (Accessor m) = Accessor m
  {-# INLINE coerce #-}
