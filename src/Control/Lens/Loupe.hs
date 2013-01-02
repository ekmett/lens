{-# LANGUAGE Rank2Types #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Loupe
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- This module is deprecated; use 'Control.Lens.Lens.ALens' instead.
-------------------------------------------------------------------------------
module Control.Lens.Loupe
  (
    Loupe, SimpleLoupe
  , storing
  , (^#)
  , ( #~ ), ( #%~ ), ( #%%~ ), (<#~), (<#%~)
  , ( #= ), ( #%= ), ( #%%= ), (<#=), (<#%=)
  ) where

import Control.Lens.Internal
import Control.Lens.Lens
import Control.Lens.Type

type Loupe s t a b = LensLike (Pretext (->) (->) a b) s t a b
{-# DEPRECATED Loupe "use ALens" #-}

-- | @type 'SimpleLoupe' = 'Simple' 'Loupe'@
type SimpleLoupe s a = Loupe s s a a
{-# DEPRECATED SimpleLoupe "use ALens'" #-}
