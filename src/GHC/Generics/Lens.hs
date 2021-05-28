{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
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
  (
    generic
  , generic1
  , _V1
  , _U1
  , _Par1
  , _Rec1
  , _K1
  , _M1
  , _L1
  , _R1
  , _UAddr
  , _UChar
  , _UDouble
  , _UFloat
  , _UInt
  , _UWord
  ) where

import           Control.Lens
import           GHC.Exts (Char(..), Double(..), Float(..),
                           Int(..), Ptr(..), Word(..))
import qualified GHC.Generics as Generic
import           GHC.Generics hiding (from, to)

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | Convert from the data type to its representation (or back)
--
-- >>> "hello"^.generic.from generic :: String
-- "hello"
generic :: (Generic a, Generic b) => Iso a b (Rep a g) (Rep b h)
generic = iso Generic.from Generic.to
{-# INLINE generic #-}

-- | Convert from the data type to its representation (or back)
generic1 :: (Generic1 f, Generic1 g) => Iso (f a) (g b) (Rep1 f a) (Rep1 g b)
generic1 = iso from1 to1
{-# INLINE generic1 #-}

_V1 :: Over p f (V1 s) (V1 t) a b
_V1 _ = absurd where
  absurd !_a = undefined
{-# INLINE _V1 #-}

_U1 :: Iso (U1 p) (U1 q) () ()
_U1 = iso (const ()) (const U1)
{-# INLINE _U1 #-}

_Par1 :: Iso (Par1 p) (Par1 q) p q
_Par1 = iso unPar1 Par1
{-# INLINE _Par1 #-}

_Rec1 :: Iso (Rec1 f p) (Rec1 g q) (f p) (g q)
_Rec1 = iso unRec1 Rec1
{-# INLINE _Rec1 #-}

_K1 :: Iso (K1 i c p) (K1 j d q) c d
_K1 = iso unK1 K1
{-# INLINE _K1 #-}

_M1 :: Iso (M1 i c f p) (M1 j d g q) (f p) (g q)
_M1 = iso unM1 M1
{-# INLINE _M1 #-}

_L1 :: Prism' ((f :+: g) a) (f a)
_L1 = prism remitter reviewer
  where
  remitter = L1
  reviewer (L1 l) = Right l
  reviewer x = Left x
{-# INLINE _L1 #-}

-- | You can access fields of `data (f :*: g) p` by using it's `Field1` and `Field2` instances

_R1 :: Prism' ((f :+: g) a) (g a)
_R1 = prism remitter reviewer
  where
  remitter = R1
  reviewer (R1 l) = Right l
  reviewer x = Left x
{-# INLINE _R1 #-}

_UAddr :: Iso (UAddr p) (UAddr q) (Ptr c) (Ptr d)
_UAddr = iso remitter reviewer
  where
  remitter (UAddr a) = Ptr a
  reviewer (Ptr a) = UAddr a
{-# INLINE _UAddr #-}

_UChar :: Iso (UChar p) (UChar q) Char Char
_UChar = iso remitter reviewer
  where
  remitter (UChar c) = C# c
  reviewer (C# c) = UChar c
{-# INLINE _UChar #-}

_UDouble :: Iso (UDouble p) (UDouble q) Double Double
_UDouble = iso remitter reviewer
  where
  remitter (UDouble d) = D# d
  reviewer (D# d) = UDouble d
{-# INLINE _UDouble #-}

_UFloat :: Iso (UFloat p) (UFloat q) Float Float
_UFloat = iso remitter reviewer
  where
  remitter (UFloat f) = F# f
  reviewer (F# f) = UFloat f
{-# INLINE _UFloat #-}

_UInt :: Iso (UInt p) (UInt q) Int Int
_UInt = iso remitter reviewer
  where
  remitter (UInt i) = I# i
  reviewer (I# i) = UInt i
{-# INLINE _UInt #-}

_UWord :: Iso (UWord p) (UWord q) Word Word
_UWord = iso remitter reviewer
  where
  remitter (UWord w) = W# w
  reviewer (W# w) = UWord w
{-# INLINE _UWord #-}
