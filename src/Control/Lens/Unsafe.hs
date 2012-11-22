{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Unsafe
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Control.Lens.Unsafe
  ( Trustworthy
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Data.Functor.Identity
import Data.Functor.Compose

-- | This class is only exported by this untrustworthy module, but is a superclass of 'Gettable'.
--
-- This is required because otherwise you could construct 'Unsafe.Coerce.unsafeCoerce' using 'Control.Lens.Internal.BazaarT.BazaarT' and
-- an illegal 'Control.Lens.Classes.Gettable' instance that uses @'Control.Lens.Classes.coerce' = 'undefined'@.

class Trustworthy (f :: * -> *)
instance Trustworthy (Const a)
instance Trustworthy Identity
instance Trustworthy f => Trustworthy (Backwards f)
instance Trustworthy g => Trustworthy (Compose f g)
