{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#ifndef MIN_VERSION_semigroupoids
#define MIN_VERSION_semigroupoids(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Instances
-- Copyright   :  (C) 2012-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module includes orphan instances for @(,)@, 'Either' and 'Const' that
-- should be supplied by base. These have moved to @semigroupoids@ as of 4.2.
----------------------------------------------------------------------------
module Control.Lens.Internal.Instances () where

import Data.Traversable.Instances ()

#if !(MIN_VERSION_semigroupoids(0,4,2))

import Control.Applicative
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable


#if !(MIN_VERSION_base(4,7,0))
import Data.Monoid
import Data.Foldable
import Data.Traversable
#endif

-------------------------------------------------------------------------------
-- Orphan Instances
-------------------------------------------------------------------------------

#if !(MIN_VERSION_base(4,7,0))
instance Foldable ((,) b) where
  foldMap f (_, a) = f a

instance Traversable ((,) b) where
  traverse f (b, a) = (,) b <$> f a

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right a) = f a

instance Traversable (Either a) where
  traverse _ (Left b) = pure (Left b)
  traverse f (Right a) = Right <$> f a

instance Foldable (Const m) where
  foldMap _ _ = mempty

instance Traversable (Const m) where
  traverse _ (Const m) = pure $ Const m
#endif

instance Foldable1 ((,) b) where
  foldMap1 f (_, a) = f a

instance Traversable1 ((,) b) where
  traverse1 f (b, a) = (,) b <$> f a

#endif
