{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
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
  -- * Pattern Synonyms
  , pattern Polar
  , pattern Real
  , pattern Imaginary
  , pattern Conjugate
  ) where

import Prelude ()

import Control.Lens
import Control.Lens.Internal.Prelude
import Data.Complex

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Complex
-- >>> import Debug.SimpleReflect
-- >>> let { a ≈ b = abs (a - b) < 1e-6; infix 4 ≈ }

-- | Access the 'realPart' of a 'Complex' number.
--
-- >>> (a :+ b)^._realPart
-- a
--
-- >>> a :+ b & _realPart *~ 2
-- a * 2 :+ b
--
-- @'_realPart' :: 'Functor' f => (a -> f a) -> 'Complex' a -> f ('Complex' a)@
_realPart :: Lens' (Complex a) a
_realPart f (a :+ b) = (:+ b) <$> f a
{-# INLINE _realPart #-}

-- | Access the 'imagPart' of a 'Complex' number.
--
-- >>> (a :+ b)^._imagPart
-- b
--
-- >>> a :+ b & _imagPart *~ 2
-- a :+ b * 2
--
-- @'_imagPart' :: 'Functor' f => (a -> f a) -> 'Complex' a -> f ('Complex' a)@
_imagPart :: Lens' (Complex a) a
_imagPart f (a :+ b) = (a :+) <$> f b
{-# INLINE _imagPart #-}

-- | This isn't /quite/ a legal 'Lens'. Notably the
--
-- @'view' l ('set' l b a) = b@
--
-- law is violated when you set a 'polar' value with 0 'magnitude' and non-zero
-- 'phase' as the 'phase' information is lost, or with a negative 'magnitude'
-- which flips the 'phase' and retains a positive 'magnitude'. So don't do
-- that!
--
-- Otherwise, this is a perfectly cromulent 'Lens'.
_polar :: RealFloat a => Iso' (Complex a) (a,a)
_polar = iso polar (uncurry mkPolar)
{-# INLINE _polar #-}

pattern Polar :: RealFloat a => a -> a -> Complex a
pattern Polar m theta <- (view _polar -> (m, theta)) where
  Polar m theta = review _polar (m, theta)

pattern Real :: (Eq a, Num a) => a -> Complex a
pattern Real r      = r :+ 0

pattern Imaginary :: (Eq a, Num a) => a -> Complex a
pattern Imaginary i = 0 :+ i

-- | Access the 'magnitude' of a 'Complex' number.
--
-- >>> (10.0 :+ 20.0) & _magnitude *~ 2
-- 20.0 :+ 40.0
--
-- This isn't /quite/ a legal 'Lens'. Notably the
--
-- @'view' l ('set' l b a) = b@
--
-- law is violated when you set a negative 'magnitude'. This flips the 'phase'
-- and retains a positive 'magnitude'. So don't do that!
--
-- Otherwise, this is a perfectly cromulent 'Lens'.
--
-- Setting the 'magnitude' of a zero 'Complex' number assumes the 'phase' is 0.
_magnitude :: RealFloat a => Lens' (Complex a) a
_magnitude f c = setMag <$> f r
  where setMag r' | r /= 0    = c * (r' / r :+ 0)
                  | otherwise = r' :+ 0
        r = magnitude c
{-# INLINE _magnitude #-}

-- | Access the 'phase' of a 'Complex' number.
--
-- >>> (mkPolar 10 (2-pi) & _phase +~ pi & view _phase) ≈ 2
-- True
--
-- This isn't /quite/ a legal 'Lens'. Notably the
--
-- @'view' l ('set' l b a) = b@
--
-- law is violated when you set a 'phase' outside the range @(-'pi', 'pi']@.
-- The phase is always in that range when queried. So don't do that!
--
-- Otherwise, this is a perfectly cromulent 'Lens'.
_phase :: RealFloat a => Lens' (Complex a) a
_phase f c = setPhase <$> f theta
  where setPhase theta' = c * cis (theta' - theta)
        theta = phase c
{-# INLINE _phase #-}

-- | Access the 'conjugate' of a 'Complex' number.
--
-- >>> (2.0 :+ 3.0) & _conjugate . _imagPart -~ 1
-- 2.0 :+ 4.0
--
-- >>> (mkPolar 10.0 2.0 ^. _conjugate . _phase) ≈ (-2.0)
-- True
_conjugate :: RealFloat a => Iso' (Complex a) (Complex a)
_conjugate = involuted conjugate
{-# INLINE _conjugate #-}

pattern Conjugate :: Num a => Complex a -> Complex a
pattern Conjugate a <- (conjugate -> a) where
  Conjugate a = conjugate a
