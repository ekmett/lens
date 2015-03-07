{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_profunctors
#define MIN_VERSION_profunctors(x,y,z) 0
#endif

#if (MIN_VERSION_profunctors(4,4,0)) && __GLASGOW_HASKELL__ >= 708
#define USE_COERCE
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
#else
{-# LANGUAGE Unsafe #-}
#endif

-- | This module provides a shim around 'coerce' that defaults to 'unsafeCoerce' on GHC < 7.8
module Control.Lens.Internal.Coerce
  ( coerce
  , coerce'
  ) where

#ifdef USE_COERCE

import Data.Coerce

coerce' :: forall a b. Coercible a b => b -> a
coerce' = coerce (id :: a -> a)
{-# INLINE coerce' #-}

#else

import Unsafe.Coerce

coerce, coerce' :: a -> b
coerce  = unsafeCoerce
coerce' = unsafeCoerce
{-# INLINE coerce #-}
{-# INLINE coerce' #-}
#endif
