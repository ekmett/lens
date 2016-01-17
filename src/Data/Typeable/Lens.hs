{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Typeable.Lens
  ( _cast
  , _gcast
  ) where

import Control.Lens
import Data.Typeable
import Data.Maybe

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | A 'Traversal'' for working with a 'cast' of a 'Typeable' value.
_cast :: (Typeable s, Typeable a) => Traversal' s a
_cast f s = case cast s of
  Just a  -> fromMaybe (error "_cast: recast failed") . cast <$> f a
  Nothing -> pure s
{-# INLINE _cast #-}

-- | A 'Traversal'' for working with a 'gcast' of a 'Typeable' value.
_gcast :: (Typeable s, Typeable a) => Traversal' (c s) (c a)
_gcast f s = case gcast s of
  Just a  -> fromMaybe (error "_gcast: recast failed") . gcast <$> f a
  Nothing -> pure s
{-# INLINE _gcast #-}
