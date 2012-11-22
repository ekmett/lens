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
    ) where


import Control.Applicative

-- | 'EvilBazaar' is like 'Control.Lens.Internal.Bazaar', except that it has an evil 'Gettable' instance
-- where @'Control.Lens.Internal.coerce' = 'Unsafe.Coerce.unsafeCoerce'@.
--
-- This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there *must* be a better way!.
--
-- This type isn't exported from the package in a way that allows anyone to
-- write 'Unsafe.Coerce.unsafeCoerce' with it. It's only used in the implementation of
-- 'Control.Lens.Traversal.taking'.
--
-- @g@ is a phantom type used in the 'Control.Lens.Internal.Gettable' instance.

newtype EvilBazaar (g :: * -> *) a s = EvilBazaar (forall f. Applicative f => (a -> f a) -> f s)

instance Functor (EvilBazaar g a) where
  fmap f (EvilBazaar k) = EvilBazaar (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (EvilBazaar g a) where
  pure a = EvilBazaar (\_ -> pure a)
  {-# INLINE pure #-}
  EvilBazaar mf <*> EvilBazaar ma = EvilBazaar (\k -> mf k <*> ma k)
  {-# INLINE (<*>) #-}

-- NB: We can't import .Internal yet, so the 'Gettable' instance is defined there
-- instead.

evilBazaar :: Applicative f => (a -> f a) -> EvilBazaar g a s -> f s
evilBazaar afb (EvilBazaar m) = m afb
{-# INLINE evilBazaar #-}
