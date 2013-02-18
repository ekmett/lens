-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Loupe
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- This module exports a minimalist API for working with lenses in highly
-- monomorphic settings.
-------------------------------------------------------------------------------
module Control.Lens.Loupe
  (
    ALens, ALens'
  , cloneLens
  , storing
  , (^#)
  , ( #~ ), ( #%~ ), ( #%%~ ), (<#~), (<#%~)
  , ( #= ), ( #%= ), ( #%%= ), (<#=), (<#%=)
  -- * Deprecated Aliases
  , Loupe, SimpleLoupe
  ) where

import Control.Lens.Internal.Context
import Control.Lens.Lens
import Control.Lens.Type

-- | This is an older alias for a type-restricted form of lens that is able to be passed around
-- in containers monomorphically.
--
-- Deprecated. This has since been renamed to 'ALens' for consistency.
type Loupe s t a b = LensLike (Pretext (->) a b) s t a b
{-# DEPRECATED Loupe "use ALens" #-}

-- | @
-- type 'SimpleLoupe' = 'Simple' 'Loupe'
-- @
--
-- Deprecated for two reasons. 'Loupe' is now 'ALens', and we no longer use the verbose @SimpleFoo@ naming
-- convention, this has since been renamed to 'ALens'' for consistency.
type SimpleLoupe s a = Loupe s s a a
{-# DEPRECATED SimpleLoupe "use ALens'" #-}
