{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.BazaarT
-- Copyright   :  (C) 2012 Edward Kmett, Shachaf Ben-Kiki
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types, KindSignatures
--
-- This module is not exported from this package. However, the 'BazaarT'
-- type (and only the type) is re-exported from @Control.Lens.Internal@.
--
----------------------------------------------------------------------------
module Control.Lens.Internal.BazaarT
  ( BazaarT(..)
  , bazaarT
  , sellT
  ) where

import Control.Applicative
import Control.Lens.Unsafe
import Control.Lens.Classes
import Unsafe.Coerce

-- | 'BazaarT' is like 'Control.Lens.Internal.Bazaar', except that it provides a questionable 'Gettable' instance
-- where @'Control.Lens.Internal.coerce' = 'Unsafe.Coerce.unsafeCoerce'@. To protect this instance it relies on
-- a combination of the fact that we do not export the tools for working with this type, beyond its type signature
-- to user code, and the fact that it borrows a proof obligation that the 'Gettable' instance is sound from another
-- 'Getter'.
--
-- For example. This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there
-- must be a better way!
--
-- @g@ is a phantom type used in the 'Control.Lens.Internal.Gettable' instance.

newtype BazaarT a b (g :: * -> *) t = BazaarT (forall f. Applicative f => (a -> f b) -> f t)

instance Functor (BazaarT a b g) where
  fmap f (BazaarT k) = BazaarT (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (BazaarT a b g) where
  pure a = BazaarT (\_ -> pure a)
  {-# INLINE pure #-}
  BazaarT mf <*> BazaarT ma = BazaarT (\k -> mf k <*> ma k)
  {-# INLINE (<*>) #-}

-- | Extract from a 'BazaarT'.
--
-- @'bazaarT' = 'flip' 'runBazaarT'@
bazaarT :: Applicative f => (a -> f b) -> BazaarT a b g t -> f t
bazaarT afb (BazaarT m) = m afb
{-# INLINE bazaarT #-}

-- | A trivial 'BazaarT'.
sellT :: a -> BazaarT a b f b
sellT i = BazaarT (\k -> k i)
{-# INLINE sellT #-}

instance Trustworthy g => Trustworthy (BazaarT a b g)

instance Gettable g => Gettable (BazaarT a b g) where
    coerce = unsafeCoerce
