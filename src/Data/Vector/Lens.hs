{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides lenses and traversals for working with generic
-- vectors.
-------------------------------------------------------------------------------
module Data.Vector.Lens
  ( toVectorOf
  -- * Isomorphisms
  , vector
  , forced
  -- * Lenses
  , sliced
  -- * Traversal of individual indices
  , ordinals
  ) where

import Control.Applicative
import Control.Lens
import Data.Vector as Vector hiding (zip, filter, indexed)
import Prelude hiding ((++), length, null, head, tail, init, last, map, reverse)
import Data.List (nub)
import Data.Monoid

-- | @sliced i n@ provides a 'Lens' that edits the @n@ elements starting
-- at index @i@ from a 'Lens'.
--
-- This is only a valid 'Lens' if you do not change the length of the
-- resulting 'Vector'.
--
-- Attempting to return a longer or shorter vector will result in
-- violations of the 'Lens' laws.
--
-- >>> Vector.fromList [1..10] ^. sliced 2 5
-- fromList [3,4,5,6,7]
--
-- >>> Vector.fromList [1..10] & sliced 2 5 . mapped .~ 0
-- fromList [1,2,0,0,0,0,0,8,9,10]
sliced :: Int -- ^ @i@ starting index
       -> Int -- ^ @n@ length
       -> Lens' (Vector a) (Vector a)
sliced i n f v = f (slice i n v) <&> \ v0 -> v // zip [i..i+n-1] (toList v0)
{-# INLINE sliced #-}

-- | Similar to 'toListOf', but returning a 'Vector'.
--
-- >>> toVectorOf both (8,15)
-- fromList [8,15]
toVectorOf :: Getting (Endo [a]) s a -> s -> Vector a
toVectorOf l s = fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Convert a list to a 'Vector' (or back)
--
-- >>> [1,2,3] ^. vector
-- fromList [1,2,3]
--
-- >>> [1,2,3] ^. vector . from vector
-- [1,2,3]
--
-- >>> Vector.fromList [0,8,15] ^. from vector . vector
-- fromList [0,8,15]
vector :: Iso [a] [b] (Vector a) (Vector b)
vector = iso fromList toList
{-# INLINE vector #-}

-- | Convert a 'Vector' to a version that doesn't retain any extra
-- memory.
forced :: Iso (Vector a) (Vector b) (Vector a) (Vector b)
forced = iso force force
{-# INLINE forced #-}

-- | This 'Traversal' will ignore any duplicates in the supplied list
-- of indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
ordinals :: [Int] -> IndexedTraversal' Int (Vector a) a
ordinals is f v = fmap (v //) $ traverse (\i -> (,) i <$> indexed f i (v ! i)) $ nub $ filter (\i -> 0 <= i && i < l) is where
  l = length v
{-# INLINE ordinals #-}

