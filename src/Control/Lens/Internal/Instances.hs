{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Instances
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module includes orphan instances for @(,)@, 'Either' and 'Const' that
-- should be supplied by base.
----------------------------------------------------------------------------
module Control.Lens.Internal.Instances () where

import Control.Applicative
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif

-------------------------------------------------------------------------------
-- Orphan Instances
-------------------------------------------------------------------------------

instance Foldable1 ((,) b) where
  foldMap1 f (_, a) = f a

instance Traversable1 ((,) b) where
  traverse1 f (b, a) = (,) b <$> f a

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
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

instance Monoid a => Monoid (Const a b) where
  mempty = Const mempty
  mappend (Const a) (Const b) = Const (mappend a b)
#endif
