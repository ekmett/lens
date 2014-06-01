{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics.Lens
-- Copyright   :  (C) 2012-14 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Note: @GHC.Generics@ exports a number of names that collide with @Control.Lens@.
--
-- You can use hiding or imports to mitigate this to an extent, and the following imports,
-- represent a fair compromise for user code:
--
-- > import Control.Lens hiding (Rep)
-- > import GHC.Generics hiding (from, to)
--
-- You can use 'generic' to replace 'GHC.Generics.from' and 'GHC.Generics.to' from @GHC.Generics@,
-- and probably won't be explicitly referencing 'Control.Lens.Representable.Rep' from @Control.Lens@
-- in code that uses generics.
--
-- This module provides compatibility with older GHC versions by using the
-- <http://hackage.haskell.org/package/generic-deriving generic-deriving>
-- package.
----------------------------------------------------------------------------
module GHC.Generics.Lens
  ( module Generics.Deriving.Lens
  ) where

import Control.Lens
import Generics.Deriving.Lens
import GHC.Generics

_V1 :: Over p f (V1 s) (V1 t) a b
_V1 _ = absurd where
  absurd !_a = undefined
{-# INLINE _V1 #-}

_U1 :: Iso' (U1 p) ()
_U1 = dimap (const ()) (fmap (const U1))
{-# INLINE _U1 #-}

_Par1 :: Iso' (Par1 p) p
_Par1 = dimap unPar1 (fmap Par1)
{-# INLINE _Par1 #-}

_Rec1 :: Iso' (Rec1 f p) (f p)
_Rec1 = dimap unRec1 (fmap Rec1)
{-# INLINE _Rec1 #-}

_K1 :: Iso' (K1 i c p) c
_K1 = dimap unK1 (fmap K1)
{-# INLINE _K1 #-}

_M1 :: Iso' (M1 i c f p) (f p)
_M1 = dimap unM1 (fmap M1)
{-# INLINE _M1 #-}

_L1 :: Prism' ((f :+: g) a) (f a)
_L1 = prism' L1 fro
  where
  fro (L1 l) = Just l
  fro _ = Nothing
{-# INLINE _L1 #-}

_R1 :: Prism' ((f :+: g) a) (g a)
_R1 = prism' R1 fro
  where
  fro (R1 l) = Just l
  fro _ = Nothing
{-# INLINE _R1 #-}
