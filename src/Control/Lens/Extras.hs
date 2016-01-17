-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A few extra names that didn't make it into Control.Lens.
--
----------------------------------------------------------------------------
module Control.Lens.Extras
  ( is
  , module Data.Data.Lens
  ) where

import Control.Lens
import Data.Data.Lens

-- $setup
-- >>> import Control.Lens
-- >>> import Numeric.Lens (hex)

-- | Check to see if this 'Prism' matches.
--
-- >>> is _Left (Right 12)
-- False
--
-- >>> is hex "3f79"
-- True
is :: APrism s t a b -> s -> Bool
is k = not . isn't k
{-# INLINE is #-}
