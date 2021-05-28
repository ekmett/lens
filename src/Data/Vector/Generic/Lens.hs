{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "lens-common.h"

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Generic.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides lenses and traversals for working with generic
-- vectors.
-------------------------------------------------------------------------------
module Data.Vector.Generic.Lens
  ( toVectorOf
  -- * Isomorphisms
  , forced
  , vector
  , asStream
  , asStreamR
  , cloned
  , converted
  -- * Lenses
  , sliced
  -- * Traversal of individual indices
  , ordinals
  , vectorIx
  , vectorTraverse
  ) where

import Prelude ()

import Control.Lens.Type
import Control.Lens.Lens
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Iso
import Control.Lens.Indexed
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Internal.List (ordinalNub)
import Control.Lens.Internal.Prelude
import Data.Vector.Fusion.Bundle (Bundle)
import qualified Data.Vector.Generic as V
import Data.Vector.Generic (Vector)
import Data.Vector.Generic.New (New)

-- $setup
-- >>> import qualified Data.Vector as Vector
-- >>> import Control.Lens

-- | @sliced i n@ provides a 'Lens' that edits the @n@ elements starting
-- at index @i@ from a 'Lens'.
--
-- This is only a valid 'Lens' if you do not change the length of the
-- resulting 'Vector'.
--
-- Attempting to return a longer or shorter vector will result in
-- violations of the 'Lens' laws.
--
-- >>> Vector.fromList [1..10] ^. sliced 2 5 == Vector.fromList [3,4,5,6,7]
-- True
--
-- >>> (Vector.fromList [1..10] & sliced 2 5 . mapped .~ 0) == Vector.fromList [1,2,0,0,0,0,0,8,9,10]
-- True
sliced :: Vector v a
       => Int -- ^ @i@ starting index
       -> Int -- ^ @n@ length
       -> Lens' (v a) (v a)
sliced i n f v = f (V.slice i n v) <&> \ v0 -> v V.// zip [i..i+n-1] (V.toList v0)
{-# INLINE sliced #-}

-- | Similar to 'toListOf', but returning a 'Vector'.
--
-- >>> (toVectorOf both (8,15) :: Vector.Vector Int) == Vector.fromList [8,15]
-- True
toVectorOf :: Vector v a => Getting (Endo [a]) s a -> s -> v a
toVectorOf l s = V.fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Convert a list to a 'Vector' (or back.)
--
-- >>> ([1,2,3] ^. vector :: Vector.Vector Int) == Vector.fromList [1,2,3]
-- True
--
-- >>> Vector.fromList [0,8,15] ^. from vector
-- [0,8,15]
vector :: (Vector v a, Vector v b) => Iso [a] [b] (v a) (v b)
vector = iso V.fromList V.toList
{-# INLINE vector #-}

-- | Convert a 'Vector' to a finite 'Bundle' (or back.)
asStream :: (Vector v a, Vector v b) => Iso (v a) (v b) (Bundle v a) (Bundle v b)
asStream = iso V.stream V.unstream
{-# INLINE asStream #-}

-- | Convert a 'Vector' to a finite 'Bundle' from right to left (or
-- back.)
asStreamR :: (Vector v a, Vector v b) => Iso (v a) (v b) (Bundle v a) (Bundle v b)
asStreamR = iso V.streamR V.unstreamR
{-# INLINE asStreamR #-}

-- | Convert a 'Vector' back and forth to an initializer that when run
-- produces a copy of the 'Vector'.
cloned :: Vector v a => Iso' (v a) (New v a)
cloned = iso V.clone V.new
{-# INLINE cloned #-}

-- | Convert a 'Vector' to a version that doesn't retain any extra
-- memory.
forced :: Vector v a => Iso' (v a) (v a)
forced = involuted V.force
{-# INLINE forced #-}

-- | This 'Traversal' will ignore any duplicates in the supplied list
-- of indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
ordinals :: Vector v a => [Int] -> IndexedTraversal' Int (v a) a
ordinals is f v = fmap (v V.//) $ traverse (\i -> (,) i <$> indexed f i (v V.! i)) $ ordinalNub (V.length v) is
{-# INLINE ordinals #-}

-- | Like 'ix' but polymorphic in the vector type.
vectorIx :: V.Vector v a => Int -> Traversal' (v a) a
vectorIx i f v
  | 0 <= i && i < V.length v = f (v V.! i) <&> \a -> v V.// [(i, a)]
  | otherwise                = pure v
{-# INLINE vectorIx #-}

-- | Indexed vector traversal for a generic vector.
vectorTraverse :: (V.Vector v a, V.Vector w b) => IndexedTraversal Int (v a) (w b) a b
vectorTraverse f v = V.fromListN (V.length v) <$> traversed f (V.toList v)
{-# INLINE [0] vectorTraverse #-}

{-# RULES
"vectorTraverse -> mapped" vectorTraverse  = sets V.map         :: (V.Vector v a, V.Vector v b) => ASetter (v a) (v b) a b;
"vectorTraverse -> imapped" vectorTraverse = isets V.imap       :: (V.Vector v a, V.Vector v b) => AnIndexedSetter Int (v a) (v b) a b;
"vectorTraverse -> foldr"  vectorTraverse  = foldring V.foldr   :: V.Vector v a => Getting (Endo r) (v a) a;
"vectorTraverse -> ifoldr" vectorTraverse  = ifoldring V.ifoldr :: V.Vector v a => IndexedGetting Int (Endo r) (v a) a;
 #-}

-- | Different vector implementations are isomorphic to each other.
converted :: (Vector v a, Vector w a, Vector v b, Vector w b) => Iso (v a) (v b) (w a) (w b)
converted = iso V.convert V.convert
