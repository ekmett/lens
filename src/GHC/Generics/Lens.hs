-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
----------------------------------------------------------------------------
module GHC.Generics.Lens
  ( generic
  , generic1
  ) where

import Control.Lens hiding (from, to)
import GHC.Generics

-- | Convert from the data type to its representation (or back)
generic :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b y)
generic = isos from to from to

-- | Convert from the data type to its representation (or back)
generic1 :: (Generic1 f, Generic1 g) => Iso (f a) (g b) (Rep1 f a) (Rep1 g b)
generic1 = isos from1 to1 from1 to1
