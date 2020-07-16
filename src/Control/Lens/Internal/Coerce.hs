{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Edward Kmett and Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provided a shim around 'coerce'.
-----------------------------------------------------------------------------
module Control.Lens.Internal.Coerce
  ( coerce
  , coerce'
  , (#..)
  ) where

import Data.Profunctor.Unsafe
import Data.Coerce

coerce' :: forall a b. Coercible a b => b -> a
#if __GLASGOW_HASKELL__ <710
coerce' = coerce (id :: a -> a)
#else
coerce' = coerce
#endif
{-# INLINE coerce' #-}

(#..) :: (Profunctor p, Coercible c b) => (b -> c) -> p a b -> p a c
(#..) = (#.)
{-# INLINE (#..) #-}
