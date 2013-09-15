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
  ) where

import Control.Applicative
import Data.Functor.Contravariant
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
