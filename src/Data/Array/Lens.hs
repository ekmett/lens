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
  , amapped
  , ixmapped
  -- * Traversal
  , traverseArray
  ) where

import Control.Applicative
import Control.Lens
import Data.Array.IArray

-- | Access an element of an array.
--
-- Note: The indexed element is assumed to exist in the target array.
--
-- > arr ! i = arr^.ix i
-- > arr // [(i,e)] = ix i ^= e $ arr
--
-- > ghci> ix 2 ^= 9 $ listArray (1,5) [4,5,6,7,8]
-- > array (1,5) [4,9,6,7,8]
ix :: (IArray a e, Ix i) => i -> Simple Lens (a i e) e
ix i f arr = (\e -> arr // [(i,e)]) <$> f (arr ! i)
{-# INLINE ix #-}

-- | This setter can be used to map over all of the values in an array.
--
-- Note: 'traverseArray' is strictly more general and permits more operations
--
-- > amap = adjust amapped
-- > amapped = sets amap
amapped :: (IArray a c, IArray a d, Ix i) => Setter (a i c) (a i d) c d
amapped = sets amap
{-# INLINE amapped #-}

-- | This setter can be used to derive a new array from an old array by
-- applying a function to each of the indices.
--
-- > ixmap = adjust . ixmapped
-- > ixmapped = sets . ixmap
ixmapped :: (IArray a e, Ix i, Ix j) => (i,i) -> Setter (a j e) (a i e) i j
ixmapped = sets . ixmap
{-# INLINE ixmapped #-}

-- | Generic 'Traversal' of the elements of an array.
--
-- > amap = adjust traverseArray
traverseArray :: (IArray a c, IArray a d, Ix i) => Traversal (a i c) (a i d) c d
traverseArray f arr = array (bounds arr) <$> (traverse._2) f (assocs arr)
{-# INLINE traverseArray #-}
