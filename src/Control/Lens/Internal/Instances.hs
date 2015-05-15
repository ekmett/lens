{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_semigroupoids
#define MIN_VERSION_semigroupoids(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Instances
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module includes orphan instances for @(,)@, 'Either' and 'Const' that
-- should be supplied by base. These have moved to @semigroupoids@ as of 4.2.
----------------------------------------------------------------------------
module Control.Lens.Internal.Instances () where

import Data.Orphans ()
import Data.Traversable.Instances ()

#if !(MIN_VERSION_semigroupoids(4,2,0))

import Control.Applicative
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

-------------------------------------------------------------------------------
-- Orphan Instances
-------------------------------------------------------------------------------

instance Foldable1 ((,) b) where
  foldMap1 f (_, a) = f a

instance Traversable1 ((,) b) where
  traverse1 f (b, a) = (,) b <$> f a

#endif
