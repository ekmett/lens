{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
    Gettable
  -- ** Getters
  , coerce
  , noEffect
  , Accessor(..)
  ) where

import Control.Applicative
import Data.Functor.Apply
import Data.Functor.Contravariant
import Data.Semigroup
import Data.Void

-- | This class is provided mostly for backwards compatibility with lens 3.8,
-- but it can also shorten type signatures.
class (Contravariant f, Functor f) => Gettable f
instance (Contravariant f, Functor f) => Gettable f

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | This Generalizes 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages.
--
-- A 'Functor' you can 'coerce' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- By the 'Functor' and 'Contravariant' laws, an instance of 'Gettable' will necessarily satisfy:
--
-- @'id' = 'fmap' f = 'coerce' = 'contramap' g@
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce a = absurd <$> contramap absurd a
{-# INLINE coerce #-}

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Contravariant f, Applicative f) => f a
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

instance Contravariant (Accessor r) where
  contramap _ (Accessor m) = Accessor m
  {-# INLINE contramap #-}

instance Semigroup r => Apply (Accessor r) where
  Accessor a <.> Accessor b = Accessor (a <> b)
  {-# INLINE (<.>) #-}

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  {-# INLINE pure #-}
  Accessor a <*> Accessor b = Accessor (mappend a b)
  {-# INLINE (<*>) #-}
