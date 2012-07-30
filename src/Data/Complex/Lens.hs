{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Complex.Lens
  ( real, imaginary, polarize
  , traverseComplex
  ) where

import Control.Applicative
import Control.Lens
import Data.Complex

-- | Access the real part of a complex number
--
-- > real :: Functor f => (a -> f a) -> Complex a -> f (Complex a)
real :: Simple Lens (Complex a) a
real f (a :+ b) = (:+ b) <$> f a

-- | Access the imaginary part of a complex number
--
-- > imaginary :: Functor f => (a -> f a) -> Complex a -> f (Complex a)
imaginary :: Simple Lens (Complex a) a
imaginary f (a :+ b) = (a :+) <$> f b

-- | This isn't /quite/ a legal lens. Notably the @view l (set l b a) = b@ law
-- is violated when you set a polar value with 0 magnitude and non-zero phase
-- as the phase information is lost. So don't do that!
--
-- Otherwise, this is a perfectly convenient lens.
--
-- > polarize :: (RealFloat a, RealFloat b, Functor f)
-- >           => ((a,a) -> f (b,b)) -> Complex a -> f (Complex b)
polarize :: (RealFloat a, RealFloat b) => Lens (Complex a) (Complex b) (a,a) (b,b)
polarize f c = uncurry mkPolar <$> f (polar c)

-- | Traverse both the real and imaginary parts of a complex number.
--
-- > traverseComplex :: Applicative f => (a -> f b) -> Complex a -> f (Complex b)
traverseComplex :: Traversal (Complex a) (Complex b) a b
traverseComplex f (a :+ b) = (:+) <$> f a <*> f b
