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
-- This module provides a shim around 'coerce'.
-- We need to work around a GHC 7.8-specific 'Coercible' solver bug(s).
-----------------------------------------------------------------------------
module Control.Lens.Internal.Coerce
  ( coerce
  , coerce'
  , (#..)
  ) where

import Data.Profunctor.Unsafe
import Data.Coerce

coerce' :: forall a b. Coercible a b => b -> a
coerce' = coerce
{-# INLINE coerce' #-}

(#..) :: (Profunctor p, Coercible c b) => (b -> c) -> p a b -> p a c
(#..) = (#.)
{-# INLINE (#..) #-}
