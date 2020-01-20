{-# LANGUAGE Rank2Types #-}

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

import Prelude ()

import Control.Lens
import Control.Lens.Internal.Prelude
import Data.Maybe (fromMaybe)
import Data.Typeable

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
