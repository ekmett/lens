{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal
-- Copyright   :  (C) 2012-15 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
-- These are some of the explicit 'Functor' instances that leak into the
-- type signatures of @Control.Lens@. You shouldn't need to import this
-- module directly for most use-cases.
--
----------------------------------------------------------------------------
module Control.Lens.Internal
  ( module Control.Lens.Internal.Bazaar
  , module Control.Lens.Internal.Context
  , module Control.Lens.Internal.Fold
  , module Control.Lens.Internal.Getter
  , module Control.Lens.Internal.Indexed
  , module Control.Lens.Internal.Iso
  , module Control.Lens.Internal.Level
  , module Control.Lens.Internal.Magma
  , module Control.Lens.Internal.Prism
  , module Control.Lens.Internal.Review
  , module Control.Lens.Internal.Setter
  , module Control.Lens.Internal.Zoom
  ) where

import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Fold
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Instances ()
import Control.Lens.Internal.Iso
import Control.Lens.Internal.Level
import Control.Lens.Internal.Magma
import Control.Lens.Internal.Prism
import Control.Lens.Internal.Review
import Control.Lens.Internal.Setter
import Control.Lens.Internal.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif
