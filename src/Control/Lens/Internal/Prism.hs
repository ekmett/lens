{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Prism
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Prism
  ( Market(..)
  , Market'
  ) where

import Data.Profunctor
import Data.Profunctor.Unsafe
#ifndef SAFE
import Unsafe.Coerce
#endif

------------------------------------------------------------------------------
-- Prism: Market
------------------------------------------------------------------------------

data Market a b s t = Market (b -> t) (s -> Either t a)

-- | @type 'Market'' a s t = 'Market' a a s t@
type Market' a = Market a a

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}
  ( #. ) _ = unsafeCoerce
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = unsafeCoerce p
  {-# INLINE ( .# ) #-}

instance Prismatic (Market a b) where
  prismatic (Market bt seta) = Market bt (either Left seta)
  {-# INLINE prismatic #-}
