{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Action
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, FDs, Rank2
--
----------------------------------------------------------------------------
module Control.Lens.Action
  ( Action
  , Effective(..)
  , ineffective
  , Effect(..)
  , Acting
  , act
  , (^!)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Getter
import Control.Lens.Iso
import Control.Monad
import Data.Functor.Identity
import Data.Monoid

type Action m a c = forall f b r d. Effective m r f => (c -> f d) -> a -> f b

class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: Isomorphic k => k (m r) (f a)

ineffective :: Effective m r f => Isomorphic k => k (f a) (m r)
ineffective = from effective

instance Effective Identity r (Accessor r) where
  effective = isomorphic (Accessor . runIdentity) (Identity . runAccessor)

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = isomorphic (Backwards . effective . liftM getDual) (liftM Dual . from effective . forwards)

newtype Effect m r a = Effect { getEffect :: m r }

instance Monad m => Functor (Effect m r) where
  fmap _ (Effect m) = Effect m

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)

instance Monad m => Gettable (Effect m r) where
  coerce (Effect m) = Effect m

instance Monad m => Effective m r (Effect m r) where
  effective = isomorphic Effect getEffect

type Acting m r a b c d = (c -> Effect m r d) -> a -> Effect m r b

infixr 8 ^!
(^!) :: Monad m => a -> Acting m c a b c d -> m c
a ^! l = getEffect (l (Effect . return) a)

act :: Monad m => (a -> m c) -> Action m a c
act amc cfd a = effective (amc a >>= from effective . cfd)
