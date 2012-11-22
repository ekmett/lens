{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Evil
-- Copyright   :  (C) 2012 Edward Kmett, Shachaf Ben-Kiki
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types, KindSignatures
--
-- This module is not exported from this package. However, the 'EvilBazaar'
-- type (and only the type) is re-exported from @Control.Lens.Internal@.
--
----------------------------------------------------------------------------
module Control.Lens.Evil
  ( EvilBazaar(..)
  , evilBazaar
  , evilSell
  ) where


import Control.Applicative

-- | 'EvilBazaar' is like 'Control.Lens.Internal.Bazaar', except that it has an evil 'Gettable' instance
-- where @'Control.Lens.Internal.coerce' = 'Unsafe.Coerce.unsafeCoerce'@.
--
-- This lets us write a suitably polymorphic and lazy
-- 'Control.Lens.Traversal.taking', for example. But there must be a better
-- way!
--
-- 'EvilBazaar' isn't exported from the package in a way that allows anyone to
-- write 'Unsafe.Coerce.unsafeCoerce' with it.
--
-- @g@ is a phantom type used in the 'Control.Lens.Internal.Gettable' instance.

newtype EvilBazaar (g :: * -> *) a b t = EvilBazaar (forall f. Applicative f => (a -> f b) -> f t)

instance Functor (EvilBazaar g a b) where
  fmap f (EvilBazaar k) = EvilBazaar (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (EvilBazaar g a b) where
  pure a = EvilBazaar (\_ -> pure a)
  {-# INLINE pure #-}
  EvilBazaar mf <*> EvilBazaar ma = EvilBazaar (\k -> mf k <*> ma k)
  {-# INLINE (<*>) #-}

-- NB: We can't import .Internal yet, so the 'Gettable' instance is defined there
-- instead.

evilBazaar :: Applicative f => (a -> f b) -> EvilBazaar g a b t -> f t
evilBazaar afb (EvilBazaar m) = m afb
{-# INLINE evilBazaar #-}

-- | A trivial 'Bazaar'.
evilSell :: a -> EvilBazaar f a b b
evilSell i = EvilBazaar (\k -> k i)
{-# INLINE evilSell #-}
