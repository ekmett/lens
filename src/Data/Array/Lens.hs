{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, Rank2Types, LiberalTypeSynonyms
--
----------------------------------------------------------------------------
module Data.Array.Lens
  (
  -- * Indexing
    ix
  -- * Setters
  , ixmapped
  -- * Traversal
  , traverseArray
  ) where

import Control.Applicative
import Control.Lens
import Data.Array.IArray hiding (index)

-- | Access an element of an array.
--
-- Note: The indexed element is assumed to exist in the target array.
--
-- > arr ! i = arr^.ix i
-- > arr // [(i,e)] = ix i ^= e $ arr
--
-- >>> ix 2 ^= 9 $ listArray (1,5) [4,5,6,7,8]
-- array (1,5) [4,9,6,7,8]
ix :: (IArray a e, Ix i) => i -> Simple Lens (a i e) e
ix i f arr = (\e -> arr // [(i,e)]) <$> f (arr ! i)
{-# INLINE ix #-}

-- | This setter can be used to derive a new array from an old array by
-- applying a function to each of the indices.
--
-- This is a /contravariant/ 'Setter'.
--
-- > ixmap = adjust . ixmapped
-- > ixmapped = sets . ixmap
--
-- > adjust (ixmapped b) f arr ! i = arr ! f i
-- > bounds (adjust (ixmapped b) f arr) = b
ixmapped :: (IArray a e, Ix i, Ix j) => (i,i) -> Setter (a j e) (a i e) i j
ixmapped = sets . ixmap
{-# INLINE ixmapped #-}

-- | Generic 'IndexedTraversal' of the elements of an array, using the index into the
-- array as the index of the traversal.
--
-- > amap = adjust traverseArray
traverseArray :: (IArray a c, IArray a d, Ix i) => IndexedTraversal i (a i c) (a i d) c d
traverseArray = index $ \f arr -> array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f i a) (assocs arr)
{-# INLINE traverseArray #-}
