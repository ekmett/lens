{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Generic.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides lenses and traversals for working with generic vectors.
-------------------------------------------------------------------------------
module Data.Vector.Generic.Lens
  ( toVectorOf
  -- * Isomorphisms
  , forced
  , vector
  , asStream
  , asStreamR
  , cloned
  , reversed
  -- * Lenses
  , _head
  , _tail
  , _last
  , _init
  , sliced
  -- * Traversal of individual indices
  , atIndex
  , atIndices
  ) where

import Control.Applicative
import Control.Lens
import Data.List (nub)
import Data.Monoid
import Data.Vector.Generic as V hiding (zip, filter)
import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)
import Prelude hiding ((++), length, head, tail, init, last, map, reverse)

-- $setup
-- >>> import Data.Vector as Vector

-- | A lens reading and writing to the 'head' of a /non-empty/ 'Vector'
--
-- Attempting to read or write to the 'head' of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> Vector.fromList [1,2,3]^._head
-- 1
_head :: Vector v a => SimpleLens (v a) a
_head f v = (\a -> v // [(0,a)]) <$> f (head v)
{-# INLINE _head #-}

-- | A 'Lens' reading and writing to the 'last' element of a /non-empty/ 'Vector'
--
-- Attempting to read or write to the 'last' element of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> Vector.fromList [1,2]^._last
-- 2
_last :: Vector v a => SimpleLens (v a) a
_last f v = (\a -> v // [(length v - 1, a)]) <$> f (last v)
{-# INLINE _last #-}

-- | A lens reading and writing to the 'tail' of a /non-empty/ 'Vector'
--
-- Attempting to read or write to the 'tail' of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> _tail .~ Vector.fromList [3,4,5] $ Vector.fromList [1,2]
-- fromList [1,3,4,5]
_tail :: Vector v a => SimpleLens (v a) (v a)
_tail f v = cons (head v) <$> f (tail v)
{-# INLINE _tail #-}

-- | A 'Lens' reading and replacing all but the a 'last' element of a /non-empty/ 'Vector'
--
-- Attempting to read or write to all but the 'last' element of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> Vector.fromList [1,2,3,4]^._init
-- fromList [1,2,3]
_init :: Vector v a => SimpleLens (v a) (v a)
_init f v = (`snoc` last v) <$> f (init v)
{-# INLINE _init #-}

-- | @sliced i n@ provides a lens that edits the @n@ elements starting at index @i@ from a lens.
--
-- This is only a valid lens if you do not change the length of the resulting 'Vector'.
--
-- Attempting to return a longer or shorter vector will result in violations of the 'Lens' laws.
sliced :: Vector v a => Int -- ^ @i@ starting index
          -> Int -- ^ @n@ length
          -> SimpleLens (v a) (v a)
sliced i n f v = (\ v0 -> v // zip [i..i+n-1] (V.toList v0)) <$> f (slice i n v)
{-# INLINE sliced #-}

-- | Similar to 'toListOf', but returning a 'Vector'.
toVectorOf :: Vector v a => Getting (Endo [a]) s t a b -> s -> v a
toVectorOf l s = fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Convert a list to a 'Vector' (or back)
vector :: Vector v a => Simple Iso [a] (v a)
vector = iso fromList V.toList
{-# INLINE vector #-}

-- | Convert a 'Vector' to a finite 'Stream' (or back)
asStream :: Vector v a => Simple Iso (v a) (Stream a)
asStream = iso stream unstream
{-# INLINE asStream #-}

-- | Convert a 'Vector' to a finite 'Stream' from right to left (or back)
asStreamR :: Vector v a => Simple Iso (v a) (Stream a)
asStreamR = iso streamR unstreamR
{-# INLINE asStreamR #-}

-- | Convert a 'Vector' back and forth to an initializer that when run produces a copy of the 'Vector'.
cloned :: Vector v a => Simple Iso (v a) (New v a)
cloned = iso clone new
{-# INLINE cloned #-}

-- | Convert a 'Vector' to a version that doesn't retain any extra memory.
forced :: Vector v a => Simple Iso (v a) (v a)
forced = iso force force
{-# INLINE forced #-}

-- | Convert a 'Vector' to a version with all the elements in the reverse order
reversed :: Vector v a => Simple Iso (v a) (v a)
reversed = iso reverse reverse
{-# INLINE reversed #-}

-- | This is a more efficient version of 'element' that works for any 'Vector'.
--
-- @atIndex n@ is only a valid 'Lens' into a 'Vector' with 'length' at least @n + 1@.
atIndex :: Vector v a => Int -> SimpleIndexedLens Int (v a) a
atIndex i = index $ \ f v -> (\ a -> v // [(i, a)]) <$> f i (v ! i)
{-# INLINE atIndex #-}

-- | This 'Traversal' will ignore any duplicates in the supplied list of indices.
--
-- >>> toListOf (atIndices [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
atIndices :: Vector v a => [Int] -> SimpleIndexedTraversal Int (v a) a
atIndices is = index $ \ f v -> let
     l = length v
     is' = nub $ filter (<l) is
  in fmap ((v //) . zip is') . traverse (uncurry f) . zip is $ fmap (v !) is'
{-# INLINE atIndices #-}
