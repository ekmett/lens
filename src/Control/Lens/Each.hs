{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif

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
import Control.Lens.Internal
import Control.Lens.Iso
import Control.Lens.Indexed as Lens (indexed)
import Control.Lens.Type
import Control.Lens.Traversal
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Complex
import Data.Functor.Identity
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

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Text.Strict.Lens as Text
-- >>> import Data.Char as Char

-- | Extract 'each' element of a (potentially monomorphic) container.
--
-- Notably, when applied to a tuple, this generalizes 'Control.Lens.Traversal.both' to arbitrary homogeneous tuples.
--
-- >>> (1,2,3) & each *~ 10
-- (10,20,30)
--
-- It can also be used on monomorphic containers like 'StrictT.Text' or 'StrictB.ByteString'.
--
-- >>> over each Char.toUpper ("hello"^.Text.packed)
-- "HELLO"
--
-- 'each' is an indexed traversal, so it can be used to access keys in many containers:
--
-- >>> itoListOf each $ Map.fromList [("hello",2),("world",4)]
-- [("hello",2),("world",4)]
--
-- >>> ("hello","world") & each.each %~ Char.toUpper
-- ("HELLO","WORLD")
class Functor f => Each i f s t a b | s -> i a, t -> i b, s b -> t, t a -> s where
  each :: Indexable i p => IndexedLensLike p f s t a b
#ifdef DEFAULT_SIGNATURES
  default each :: (Indexable Int p, Applicative f, Traversable g, s ~ g a, t ~ g b) => IndexedLensLike p f s t a b
  each = traversed
  {-# INLINE each #-}
#endif

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a) (b,b) a b@
instance (Applicative f, a~a', b~b') => Each Int f (a,a') (b,b') a b where
  each f ~(a,b) = (,) <$> f' (0 :: Int) a <*> f' 1 b
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a) (b,b,b) a b@
instance (Applicative f, a~a2, a~a3, b~b2, b~b3) => Each Int f (a,a2,a3) (b,b2,b3) a b where
  each f ~(a,b,c) = (,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a) (b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, b~b2, b~b3, b~b4) => Each Int f (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each f ~(a,b,c,d) = (,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a) (b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, b~b2, b~b3, b~b4, b~b5) => Each Int f (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each f ~(a,b,c,d,e) = (,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a) (b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, b~b2, b~b3, b~b4, b~b5, b~b6) => Each Int f (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each f ~(a,b,c,d,e,g) = (,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a,a) (b,b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7) => Each Int f (a,a2,a3,a4,a5,a6,a7) (b,b2,b3,b4,b5,b6,b7) a b where
  each f ~(a,b,c,d,e,g,h) = (,,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g <*> f' 6 h
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a,a,a) (b,b,b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8) => Each Int f (a,a2,a3,a4,a5,a6,a7,a8) (b,b2,b3,b4,b5,b6,b7,b8) a b where
  each f ~(a,b,c,d,e,g,h,i) = (,,,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g <*> f' 6 h <*> f' 7 i
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a,a,a,a) (b,b,b,b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, a~a9, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8, b~b9) => Each Int f (a,a2,a3,a4,a5,a6,a7,a8,a9) (b,b2,b3,b4,b5,b6,b7,b8,b9) a b where
  each f ~(a,b,c,d,e,g,h,i,j) = (,,,,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g <*> f' 6 h <*> f' 7 i <*> f' 8 j
    where f' = Lens.indexed f
  {-# INLINE each #-}

#if MIN_VERSION_base(4,4,0)
-- | @'each' :: ('RealFloat' a, 'RealFloat' b) => 'IndexedTraversal' 'Int' ('Complex' a) ('Complex' b) a b@
instance (Applicative f, RealFloat a, RealFloat b) => Each Int f (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f' (0 :: Int) a <*> f' 1 b
    where f' = Lens.indexed f
  {-# INLINE each #-}
#else
-- | @'each' :: 'IndexedTraversal' 'Int' ('Complex' a) ('Complex' b) a b@
instance Applicative f => Each Int f (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f' (0 :: Int) a <*> f' (1 :: Int) b
    where f' = Lens.indexed f
  {-# INLINE each #-}
#endif

-- | @'each' :: 'IndexedTraversal' c ('Map' c a) ('Map' c b) a b@
instance Applicative f => Each c f (Map c a) (Map c b) a b where
  each f m = sequenceA $ Map.mapWithKey f' m
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Map' c a) ('Map' c b) a b@
instance Applicative f => Each Int f (IntMap a) (IntMap b) a b where
  each f m = sequenceA $ IntMap.mapWithKey f' m
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' c ('HashMap' c a) ('HashMap' c b) a b@
instance Applicative f => Each c f (HashMap c a) (HashMap c b) a b where
  each = HashMap.traverseWithKey . Lens.indexed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' [a] [b] a b@
instance Applicative f => Each Int f [a] [b] a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Identity' a) ('Identity' b) a b@
instance Functor f => Each Int f (Identity a) (Identity b) a b where
  each f (Identity a) = Identity <$> Lens.indexed f (0 :: Int) a
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Maybe' a) ('Maybe' b) a b@
instance Applicative f => Each Int f (Maybe a) (Maybe b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Seq' a) ('Seq' b) a b@
instance Applicative f => Each Int f (Seq a) (Seq b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Tree' a) ('Tree' b) a b@
instance Applicative f => Each Int f (Tree a) (Tree b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Vector.Vector' a) ('Vector.Vector' b) a b@
instance Applicative f => Each Int f (Vector.Vector a) (Vector.Vector b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: ('Prim' a, 'Prim' b) => 'IndexedTraversal' 'Int' ('Prim.Vector' a) ('Prim.Vector' b) a b@
instance (Applicative f, Prim a, Prim b) => Each Int f (Prim.Vector a) (Prim.Vector b) a b where
  each f v = Prim.fromListN (Prim.length v) <$> traversed (Indexed f') (Prim.toList v)
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: ('Storable' a, 'Storable' b) => 'IndexedTraversal' 'Int' ('Storable.Vector' a) ('Storable.Vector' b) a b@
instance (Applicative f, Storable a, Storable b) => Each Int f (Storable.Vector a) (Storable.Vector b) a b where
  each f v = Storable.fromListN (Storable.length v) <$> traversed (Indexed f') (Storable.toList v)
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: ('Unbox' a, 'Unbox' b) => 'IndexedTraversal' 'Int' ('Unboxed.Vector' a) ('Unboxed.Vector' b) a b@
instance (Applicative f, Unbox a, Unbox b) => Each Int f (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each f v = Unboxed.fromListN (Unboxed.length v) <$> traversed (Indexed f') (Unboxed.toList v)
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' 'StrictT.Text' 'StrictT.Text' 'Char' 'Char'@
instance Applicative f =>  Each Int f StrictT.Text StrictT.Text Char Char where
  each = iso StrictT.unpack StrictT.pack . traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int64' 'LazyT.Text' 'LazyT.Text' 'Char' 'Char'@
instance Applicative f => Each Int64 f LazyT.Text LazyT.Text Char Char where
  each = iso LazyT.unpack LazyT.pack . traversed64
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' 'StrictB.ByteString' 'StrictB.ByteString' 'Char' 'Char'@
instance Applicative f => Each Int f StrictB.ByteString StrictB.ByteString Word8 Word8 where
  each = iso StrictB.unpack StrictB.pack . traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int64' 'LazyB.ByteString' 'LazyB.ByteString' 'Char' 'Char'@
instance Applicative f => Each Int64 f LazyB.ByteString LazyB.ByteString Word8 Word8 where
  each = iso LazyB.unpack LazyB.pack . traversed64
  {-# INLINE each #-}

-- | @'each' :: 'Ix' i => 'IndexedTraversal' i ('Array' i a) ('Array' i b) a b@
instance (Applicative f, Ix i) => Each i f (Array i a) (Array i b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> Lens.indexed f i a) (IArray.assocs arr)
  {-# INLINE each #-}

-- | @'each' :: ('Ix' i, 'IArray' 'UArray' a, 'IArray' 'UArray' b) => 'IndexedTraversal' i ('Array' i a) ('Array' i b) a b@
instance (Applicative f, Ix i, IArray UArray a, IArray UArray b) => Each i f (UArray i a) (UArray i b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> Lens.indexed f i a) (IArray.assocs arr)
  {-# INLINE each #-}

-- | @'each' :: 'Control.Lens.IndexedSetter.IndexedSetter' i (i -> a) (i -> b) a b@
instance Settable f => Each i f (i -> a) (i -> b) a b where
  each f g = pure (\i -> untaintedDot (Lens.indexed f i) (g i))
  {-# INLINE each #-}
