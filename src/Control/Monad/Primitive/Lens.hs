{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Primitive.Lens
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Control.Monad.Primitive
--
----------------------------------------------------------------------------
module Control.Monad.Primitive.Lens
  (
    prim
  ) where

import Control.Lens
import Control.Monad.Primitive (PrimMonad(..))
import GHC.Prim (State#)

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}

prim :: (PrimMonad m) => Iso' (m a) (State# (PrimState m) -> (# State# (PrimState m), a #))
prim = iso internal primitive
{-# INLINE prim #-}
