{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 708
#define USE_COERCE
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
#else
{-# LANGUAGE Unsafe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Edward Kmett and Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides a shim around 'coerce' that defaults to 'unsafeCoerce'
-- on GHC < 7.8. It also exposes a type-restricted version of '(#.)' that
-- works around a bizarre GHC 7.10–specific bug.
-----------------------------------------------------------------------------
module Control.Lens.Internal.Coerce
  ( coerce
  , coerce'
  , (#..)
  ) where

import Data.Profunctor.Unsafe

#ifdef USE_COERCE

import Data.Coerce

coerce' :: forall a b. Coercible a b => b -> a
coerce' = coerce (id :: a -> a)
{-# INLINE coerce' #-}

(#..) :: (Profunctor p, Coercible c b) => (b -> c) -> p a b -> p a c
(#..) = (#.)
{-# INLINE (#..) #-}

#else

import Unsafe.Coerce

coerce, coerce' :: a -> b
coerce  = unsafeCoerce
coerce' = unsafeCoerce
{-# INLINE coerce #-}
{-# INLINE coerce' #-}

(#..) :: Profunctor p => (b -> c) -> p a b -> p a c
(#..) = (#.)
{-# INLINE (#..) #-}
#endif
