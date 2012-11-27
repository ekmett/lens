{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
-- Lenses and traversals for complex numbers
--
----------------------------------------------------------------------------
module Data.Complex.Lens
  ( _realPart
  , _imagPart
  , _polar
  , _magnitude
  , _phase
  , _conjugate
  , complex
  ) where

import Control.Applicative
import Control.Lens
import Data.Complex

-- $setup
-- >>> let { a ≈ b = abs (a - b) < 1e-6; infix 4 ≈ }

-- | Access the 'realPart' of a 'Complex' number
--
-- >>> (1.0 :+ 0.0)^._realPart
-- 1.0
--
-- >>> 3 :+ 1 & _realPart *~ 2
-- 6 :+ 1
--
-- @'_realPart' :: 'Functor' f => (a -> f a) -> 'Complex' a -> f ('Complex' a)@
#if MIN_VERSION_base(4,4,0)
_realPart :: Simple Lens (Complex a) a
#else
_realPart :: RealFloat a => Simple Lens (Complex a) a
#endif
_realPart f (a :+ b) = (:+ b) <$> f a

-- | Access the 'imagPart' of a 'Complex' number
--
-- >>> (0.0 :+ 1.0)^._imagPart
-- 1.0
--
-- @'_imagPart' :: 'Functor' f => (a -> f a) -> 'Complex' a -> f ('Complex' a)@
#if MIN_VERSION_base(4,4,0)
_imagPart :: Simple Lens (Complex a) a
#else
_imagPart :: RealFloat a => Simple Lens (Complex a) a
#endif
_imagPart f (a :+ b) = (a :+) <$> f b

-- | This isn't /quite/ a legal lens. Notably the
--
-- @'view' l ('set' l b a) = b@
--
-- law is violated when you set a 'polar' value with 0 'magnitude' and non-zero
-- 'phase' as the 'phase' information is lost, or with a negative 'magnitude'
-- which flips the 'phase' and retains a positive 'magnitude'. So don't do
-- that!
--
-- Otherwise, this is a perfectly cromulent 'Lens'.
_polar :: RealFloat a => Simple Iso (Complex a) (a,a)
_polar = iso polar (uncurry mkPolar)

-- | Access the 'magnitude' of a 'Complex' number
--
-- >>> (10.0 :+ 20.0) & _magnitude *~ 2
-- 20.0 :+ 40.0
--
-- This isn't /quite/ a legal lens. Notably the
--
-- @'view' l ('set' l b a) = b@
--
-- law is violated when you set a negative 'magnitude'. This flips the 'phase'
-- and retains a positive 'magnitude'. So don't do that!
--
-- Otherwise, this is a perfectly cromulent 'Lens'.
--
-- Setting the 'magnitude' of a zero 'Complex' number assumes the 'phase' is 0.
_magnitude :: RealFloat a => Simple Lens (Complex a) a
_magnitude f c = setMag <$> f r
  where setMag r' | r /= 0    = c * (r' / r :+ 0)
                  | otherwise = r' :+ 0
        r = magnitude c

-- | Access the 'phase' of a 'Complex' number
--
-- >>> (mkPolar 10 (2-pi) & _phase +~ pi & view _phase) ≈ 2
-- True
--
-- This isn't /quite/ a legal lens. Notably the
--
-- @'view' l ('set' l b a) = b@
--
-- law is violated when you set a 'phase' outside the range @(-'pi', 'pi']@.
-- The phase is always in that range when queried. So don't do that!
--
-- Otherwise, this is a perfectly cromulent 'Lens'.
_phase :: RealFloat a => Simple Lens (Complex a) a
_phase f c = setPhase <$> f theta
  where setPhase theta' = c * cis (theta' - theta)
        theta = phase c

-- | Access the 'conjugate' of a 'Complex' number
--
-- >>> (2.0 :+ 3.0) & _conjugate . _imagPart -~ 1
-- 2.0 :+ 4.0
--
-- >>> (mkPolar 10.0 2.0 ^. _conjugate . _phase) ≈ (-2.0)
-- True
_conjugate :: RealFloat a => Simple Iso (Complex a) (Complex a)
_conjugate = iso conjugate conjugate

-- | Traverse both the 'realPart' and the 'imagPart' of a 'Complex' number.
--
-- >>> 0 & complex .~ 1
-- 1 :+ 1
--
-- >>> 3 :+ 4 & complex *~ 2
-- 6 :+ 8
--
-- >>> sumOf complex (1 :+ 2)
-- 3
--
-- @'complex' :: 'Applicative' f => (a -> f b) -> 'Complex' a -> f ('Complex' b)@
#if MIN_VERSION_base(4,4,0)
complex :: Traversal (Complex a) (Complex b) a b
#else
complex :: (RealFloat a, RealFloat b) => Traversal (Complex a) (Complex b) a b
#endif
complex f (a :+ b) = (:+) <$> f a <*> f b
