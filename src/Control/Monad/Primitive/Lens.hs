{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}

#ifndef MIN_VERSION_primitive
#define MIN_VERSION_primitive(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Primitive.Lens
-- Copyright   :  (C) 2014 Edward Kmett
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
import Control.Monad.Primitive
import GHC.Prim (State#)

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}

#if MIN_VERSION_primitive(0,6,0)
prim :: PrimBase m => Iso' (m a) (State# (PrimState m) -> (# State# (PrimState m), a #))
#else
prim :: PrimMonad m => Iso' (m a) (State# (PrimState m) -> (# State# (PrimState m), a #))
#endif
prim = iso internal primitive
{-# INLINE prim #-}
