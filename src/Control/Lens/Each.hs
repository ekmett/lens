{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Each
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.Each
  ( Each(..)
  ) where

import Control.Applicative
import Control.Lens.Indexed as Lens
import Control.Lens.IndexedTraversal
import Control.Lens.Traversal
import Control.Lens.Iso
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Complex
import Data.Int
import Data.Sequence as Seq
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Traversable
import Data.Tree as Tree
import Data.Word
import Data.Map as Map
import Data.IntMap as IntMap
import Data.HashMap.Lazy as HashMap
import Data.Vector as Vector
import Data.Vector.Primitive as Prim
import Data.Vector.Storable as Storable
import Data.Vector.Unboxed as Unboxed
import Data.Array.Unboxed as Unboxed
import Data.Array.IArray as IArray

-- | Extract 'each' element of a (potentially monomorphic) container.
class Each i s t a b | s -> i a, t -> i b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable f, s ~ f a, t ~ f b) => IndexedTraversal Int s t a b
  each = traversed
  {-# INLINE each #-}

instance (a ~ a', b ~ b') => Each Int (a,a') (b,b') a b where
  each = Lens.indexed $ \ f ~(a,b) -> (,) <$> f (0 :: Int) a <*> f 1 b
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, b ~ b2, b ~ b3) => Each Int (a,a2,a3) (b,b2,b3) a b where
  each = Lens.indexed $ \ f ~(a,b,c) -> (,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, b ~ b2, b ~ b3, b ~ b4) => Each Int (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each = Lens.indexed $ \ f ~(a,b,c,d) -> (,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, b ~ b2, b ~ b3, b ~ b4, b ~ b5) => Each Int (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each = Lens.indexed $ \ f ~(a,b,c,d,e) -> (,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6) => Each Int (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each = Lens.indexed $ \ f ~(a,b,c,d,e,g) -> (,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7) => Each Int (a,a2,a3,a4,a5,a6,a7) (b,b2,b3,b4,b5,b6,b7) a b where
  each = Lens.indexed $ \ f ~(a,b,c,d,e,g,h) -> (,,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g <*> f 6 h
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8) => Each Int (a,a2,a3,a4,a5,a6,a7,a8) (b,b2,b3,b4,b5,b6,b7,b8) a b where
  each = Lens.indexed $ \ f ~(a,b,c,d,e,g,h,i) -> (,,,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g <*> f 6 h <*> f 7 i
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9) => Each Int (a,a2,a3,a4,a5,a6,a7,a8,a9) (b,b2,b3,b4,b5,b6,b7,b8,b9) a b where
  each = Lens.indexed $ \ f ~(a,b,c,d,e,g,h,i,j) -> (,,,,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g <*> f 6 h <*> f 7 i <*> f 8 j
  {-# INLINE each #-}

#if MIN_VERSION_base(4,4,0)
instance RealFloat a => Each Int (Complex a) (Complex a) a a where
  each = Lens.indexed $ \ f (a :+ b) -> (:+) <$> f (0 :: Int) a <*> f 1 b
  {-# INLINE each #-}
#else
instance Each Int (Complex a) (Complex a) a a where
  each = Lens.indexed $ \ f (a :+ b) -> (:+) <$> f (0 :: Int) a <*> f 1 b
  {-# INLINE each #-}
#endif

instance Each k (Map k a) (Map k b) a b where
  each = Lens.indexed $ \f m -> sequenceA $ Map.mapWithKey f m
  {-# INLINE each #-}

instance Each Int (IntMap a) (IntMap b) a b where
  each = Lens.indexed $ \f m -> sequenceA $ IntMap.mapWithKey f m
  {-# INLINE each #-}

instance Each k (HashMap k a) (HashMap k b) a b where
  each = Lens.indexed HashMap.traverseWithKey
  {-# INLINE each #-}

instance Each Int [a] [b] a b
instance Each Int (Seq a) (Seq b) a b
instance Each Int (Tree a) (Tree b) a b
instance Each Int (Vector.Vector a) (Vector.Vector b) a b

instance (Prim a, Prim b) => Each Int (Prim.Vector a) (Prim.Vector b) a b where
  each = Lens.indexed $ \f v -> Prim.fromListN (Prim.length v) <$> withIndex traversed f (Prim.toList v)

instance (Storable a, Storable b) => Each Int (Storable.Vector a) (Storable.Vector b) a b where
  each = Lens.indexed $ \f v -> Storable.fromListN (Storable.length v) <$> withIndex traversed f (Storable.toList v)

instance (Unbox a, Unbox b) => Each Int (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each = Lens.indexed $ \f v -> Unboxed.fromListN (Unboxed.length v) <$> withIndex traversed f (Unboxed.toList v)

instance Each Int StrictT.Text StrictT.Text Char Char where
  each = iso StrictT.unpack StrictT.pack .> traversed
  {-# INLINE each #-}

instance Each Int64 LazyT.Text LazyT.Text Char Char where
  each = iso LazyT.unpack LazyT.pack .> traversed64
  {-# INLINE each #-}

instance Each Int StrictB.ByteString StrictB.ByteString Word8 Word8 where
  each = iso StrictB.unpack StrictB.pack .> traversed
  {-# INLINE each #-}

instance Each Int64 LazyB.ByteString LazyB.ByteString Word8 Word8 where
  each = iso LazyB.unpack LazyB.pack .> traversed64 where
  {-# INLINE each #-}

instance (Ix i, IArray UArray a, IArray UArray b) => Each i (Array i a) (Array i b) a b where
  each = Lens.indexed $ \f arr -> array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f i a) (IArray.assocs arr)
  {-# INLINE each #-}

instance (Ix i, IArray UArray a, IArray UArray b) => Each i (UArray i a) (UArray i b) a b where
  each = Lens.indexed $ \f arr -> array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f i a) (IArray.assocs arr)
  {-# INLINE each #-}
