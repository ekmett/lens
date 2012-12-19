{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
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
  , ordinals
  ) where

import Control.Applicative
import Control.Lens
import Data.List (nub)
import Data.Monoid
import Data.Vector.Generic as V hiding (zip, filter, indexed)
import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)
import Prelude hiding ((++), length, null, head, tail, init, last, map, reverse)

-- $setup
-- >>> import Data.Vector as Vector

-- | A 'Traversal' reading and writing to the 'head' of a 'Vector'
--
-- >>> Vector.fromList [1,2,3] ^? _head
-- Just 1
--
-- >>> Vector.empty ^? _head
-- Nothing
--
-- >>> Vector.fromList "abc" & _head .~ 'Q'
-- fromList "Qbc"
_head :: Vector v a => Traversal' (v a) a
_head f v
  | null v    = pure v
  | otherwise = f (unsafeHead v) <&> \a -> v // [(0,a)]
{-# INLINE _head #-}

-- | A 'Traversal' reading and writing to the 'last' element of a 'Vector'
--
-- >>> Vector.fromList "abcde" ^? _last
-- Just 'e'
--
-- >>> Vector.empty ^? _last
-- Nothing
--
-- >>> Vector.fromList "abcde" & _last .~ 'Q'
-- fromList "abcdQ"
_last :: Vector v a => Traversal' (v a) a
_last f v
  | null v    = pure v
  | otherwise = f (unsafeLast v) <&> \a -> v // [(length v - 1, a)]
{-# INLINE _last #-}

-- | A 'Traversal' reading and writing to the 'tail' of a 'Vector'
--
-- >>> Vector.fromList "abcde" ^? _tail
-- Just (fromList "bcde")
--
-- >>> Vector.empty ^? _tail
-- Nothing
--
-- >>> _tail .~ Vector.fromList [3,4,5] $ Vector.fromList [1,2]
-- fromList [1,3,4,5]
_tail :: Vector v a => Traversal' (v a) (v a)
_tail f v
  | null v    = pure v
  | otherwise = f (unsafeTail v) <&> cons (unsafeHead v)
{-# INLINE _tail #-}

-- | A 'Traversal' reading and replacing all but the a 'last' element of a 'Vector'
--
-- >>> Vector.fromList [1,2,3,4] ^? _init
-- Just (fromList [1,2,3])
--
-- >>> Vector.empty ^? _init
-- Nothing
--
-- >>> Vector.fromList [1,2,3,4] ^? _init
-- Just (fromList [1,2,3])
_init :: Vector v a => Traversal' (v a) (v a)
_init f v
  | null v    = pure v
  | otherwise = f (unsafeInit v) <&> (`snoc` unsafeLast v)
{-# INLINE _init #-}

-- | @sliced i n@ provides a lens that edits the @n@ elements starting at index @i@ from a lens.
--
-- This is only a valid lens if you do not change the length of the resulting 'Vector'.
--
-- Attempting to return a longer or shorter vector will result in violations of the 'Lens' laws.
--
-- >>> Vector.fromList [1..10] ^. sliced 2 5
-- fromList [3,4,5,6,7]
--
-- >>> Vector.fromList [1..10] & sliced 2 5 . mapped .~ 0
-- fromList [1,2,0,0,0,0,0,8,9,10]
sliced :: Vector v a => Int -- ^ @i@ starting index
          -> Int -- ^ @n@ length
          -> Lens' (v a) (v a)
sliced i n f v = f (slice i n v) <&> \ v0 -> v // zip [i..i+n-1] (V.toList v0)
{-# INLINE sliced #-}

-- | Similar to 'toListOf', but returning a 'Vector'.
--
-- >>> toVectorOf both (8,15) :: Vector.Vector Int
-- fromList [8,15]
toVectorOf :: Vector v a => Getting (Endo [a]) s t a b -> s -> v a
toVectorOf l s = fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Convert a list to a 'Vector' (or back)
--
-- >>> [1,2,3] ^. vector :: Vector.Vector Int
-- fromList [1,2,3]
--
-- >>> Vector.fromList [0,8,15] ^. from vector
-- [0,8,15]
vector :: Vector v a => Iso' [a] (v a)
vector = iso fromList V.toList
{-# INLINE vector #-}

-- | Convert a 'Vector' to a finite 'Stream' (or back)
asStream :: Vector v a => Iso' (v a) (Stream a)
asStream = iso stream unstream
{-# INLINE asStream #-}

-- | Convert a 'Vector' to a finite 'Stream' from right to left (or back)
asStreamR :: Vector v a => Iso' (v a) (Stream a)
asStreamR = iso streamR unstreamR
{-# INLINE asStreamR #-}

-- | Convert a 'Vector' back and forth to an initializer that when run produces a copy of the 'Vector'.
cloned :: Vector v a => Iso' (v a) (New v a)
cloned = iso clone new
{-# INLINE cloned #-}

-- | Convert a 'Vector' to a version that doesn't retain any extra memory.
forced :: Vector v a => Iso' (v a) (v a)
forced = iso force force
{-# INLINE forced #-}

-- | Convert a 'Vector' to a version with all the elements in the reverse order
--
-- >>> Vector.fromList [1,2,3] ^. reversed
-- fromList [3,2,1]
reversed :: Vector v a => Iso' (v a) (v a)
reversed = iso reverse reverse
{-# INLINE reversed #-}

-- | This 'Traversal' will ignore any duplicates in the supplied list of indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
ordinals :: Vector v a => [Int] -> IndexedTraversal' Int (v a) a
ordinals is = indexed $ \ f v -> let
     l = length v
     is' = nub $ filter (<l) is
  in fmap ((v //) . zip is') . traverse (uncurry f) . zip is $ fmap (v !) is'
{-# INLINE ordinals #-}
