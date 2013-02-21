{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.Each
  (
  -- * Indices
    Index
  -- * Each
  , Each(..)
  ) where

import Control.Applicative
import Control.Lens.Cons as Lens
import Control.Lens.Internal.Deque
import Control.Lens.Internal.Setter
import Control.Lens.Indexed as Lens
import Control.Lens.Iso
import Control.Lens.Type
import Control.Lens.Traversal
import Data.Array.Unboxed as Unboxed
import Data.Array.IArray as IArray
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Complex
import Data.Foldable as Foldable
import Data.Functor.Identity
import Data.HashMap.Lazy as HashMap
import Data.HashSet
import Data.Int
import Data.IntMap as IntMap
import Data.IntSet
import Data.Map as Map
import Data.Set
import Data.Sequence as Seq
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Traversable
import Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Primitive as Prim
import Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable as Storable
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Unboxed as Unboxed
import Data.Vector.Unboxed (Unbox)
import Data.Word

-- | This is a common 'Index' type shared by 'Each', 'Control.Lens.At.At', 'Control.Lens.At.Contains' and 'Control.Lens.At.Ixed'.
type family Index (s :: *) :: *
type instance Index (e -> a) = e
type instance Index IntSet = Int
type instance Index (Set a) = a
type instance Index (HashSet a) = a
type instance Index [a] = Int
type instance Index (Seq a) = Int
type instance Index (a,b) = Int
type instance Index (a,b,c) = Int
type instance Index (a,b,c,d) = Int
type instance Index (a,b,c,d,e) = Int
type instance Index (a,b,c,d,e,f) = Int
type instance Index (a,b,c,d,e,f,g) = Int
type instance Index (a,b,c,d,e,f,g,h) = Int
type instance Index (a,b,c,d,e,f,g,h,i) = Int
type instance Index (IntMap a) = Int
type instance Index (Map k a) = k
type instance Index (HashMap k a) = k
type instance Index (Array i e) = i
type instance Index (UArray i e) = i
type instance Index (Vector.Vector a) = Int
type instance Index (Prim.Vector a) = Int
type instance Index (Storable.Vector a) = Int
type instance Index (Unboxed.Vector a) = Int
type instance Index (Complex a) = Int
type instance Index (Identity a) = ()
type instance Index (Maybe a) = ()
type instance Index (Tree a) = [Int]
type instance Index StrictT.Text = Int
type instance Index LazyT.Text = Int64
type instance Index StrictB.ByteString = Int
type instance Index LazyB.ByteString = Int64

-- $setup
-- >>> :set -XNoOverloadedStrings
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
-- 'each' is an 'IndexedTraversal', so it can be used to access keys in many containers:
--
-- >>> itoListOf each $ Map.fromList [("hello",2),("world",4)]
-- [("hello",2),("world",4)]
--
-- >>> ("hello","world") & each.each %~ Char.toUpper
-- ("HELLO","WORLD")
class (Functor f, Index s ~ Index t) => Each f s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: IndexedLensLike (Index s) f s t a b
#ifdef DEFAULT_SIGNATURES
  default each :: (Applicative f, Traversable g, s ~ g a, t ~ g b, Index s ~ Int, Index t ~ Int) => IndexedLensLike Int f s t a b
  each = traversed
  {-# INLINE each #-}
#endif

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a) (b,b) a b@
instance (Applicative f, a~a', b~b') => Each f (a,a') (b,b') a b where
  each f ~(a,b) = (,) <$> f' (0 :: Int) a <*> f' 1 b
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a) (b,b,b) a b@
instance (Applicative f, a~a2, a~a3, b~b2, b~b3) => Each f (a,a2,a3) (b,b2,b3) a b where
  each f ~(a,b,c) = (,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a) (b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, b~b2, b~b3, b~b4) => Each f (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each f ~(a,b,c,d) = (,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a) (b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, b~b2, b~b3, b~b4, b~b5) => Each f (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each f ~(a,b,c,d,e) = (,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a) (b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, b~b2, b~b3, b~b4, b~b5, b~b6) => Each f (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each f ~(a,b,c,d,e,g) = (,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a,a) (b,b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7) => Each f (a,a2,a3,a4,a5,a6,a7) (b,b2,b3,b4,b5,b6,b7) a b where
  each f ~(a,b,c,d,e,g,h) = (,,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g <*> f' 6 h
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a,a,a) (b,b,b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8) => Each f (a,a2,a3,a4,a5,a6,a7,a8) (b,b2,b3,b4,b5,b6,b7,b8) a b where
  each f ~(a,b,c,d,e,g,h,i) = (,,,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g <*> f' 6 h <*> f' 7 i
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' (a,a,a,a,a,a,a,a,a) (b,b,b,b,b,b,b,b,b) a b@
instance (Applicative f, a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, a~a9, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8, b~b9) => Each f (a,a2,a3,a4,a5,a6,a7,a8,a9) (b,b2,b3,b4,b5,b6,b7,b8,b9) a b where
  each f ~(a,b,c,d,e,g,h,i,j) = (,,,,,,,,) <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d <*> f' 4 e <*> f' 5 g <*> f' 6 h <*> f' 7 i <*> f' 8 j
    where f' = Lens.indexed f
  {-# INLINE each #-}

#if MIN_VERSION_base(4,4,0)
-- | @'each' :: ('RealFloat' a, 'RealFloat' b) => 'IndexedTraversal' 'Int' ('Complex' a) ('Complex' b) a b@
instance Applicative f => Each f (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f' (0 :: Int) a <*> f' (1 :: Int) b
    where f' = Lens.indexed f
  {-# INLINE each #-}
#else
-- | @'each' :: 'IndexedTraversal' 'Int' ('Complex' a) ('Complex' b) a b@
instance (Applicative f, RealFloat a, RealFloat b) => Each f (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f' (0 :: Int) a <*> f' 1 b
    where f' = Lens.indexed f
  {-# INLINE each #-}
#endif

-- | @'each' :: 'IndexedTraversal' c ('Map' c a) ('Map' c b) a b@
instance Applicative f => Each f (Map c a) (Map c b) a b where
  each f m = sequenceA $ Map.mapWithKey f' m
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' c ('Map' c a) ('Map' c b) a b@
instance Applicative f => Each f (IntMap a) (IntMap b) a b where
  each f m = sequenceA $ IntMap.mapWithKey f' m
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' c ('HashMap' c a) ('HashMap' c b) a b@
instance Applicative f => Each f (HashMap c a) (HashMap c b) a b where
  each = HashMap.traverseWithKey . Lens.indexed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' [a] [b] a b@
instance Applicative f => Each f [a] [b] a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' () ('Identity' a) ('Identity' b) a b@
instance Functor f => Each f (Identity a) (Identity b) a b where
  each f (Identity a) = Identity <$> Lens.indexed f () a
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' () ('Maybe' a) ('Maybe' b) a b@
instance Applicative f => Each f (Maybe a) (Maybe b) a b where
  each f (Just a) = Just <$> Lens.indexed f () a
  each _ Nothing  = pure Nothing
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Seq' a) ('Seq' b) a b@
instance Applicative f => Each f (Seq a) (Seq b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' ['Int'] ('Tree' a) ('Tree' b) a b@
instance Applicative f => Each f (Tree a) (Tree b) a b where
  each pafb = go (BD 0 [] 0 []) where
    go dq (Node a as) = Node <$> Lens.indexed pafb (Foldable.toList dq) a <*> itraverse (\i n -> go (Lens.snoc dq i) n) as
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' ('Vector.Vector' a) ('Vector.Vector' b) a b@
instance Applicative f => Each f (Vector.Vector a) (Vector.Vector b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: ('Prim' a, 'Prim' b) => 'IndexedTraversal' 'Int' ('Prim.Vector' a) ('Prim.Vector' b) a b@
instance (Applicative f, Prim a, Prim b) => Each f (Prim.Vector a) (Prim.Vector b) a b where
  each f v = Prim.fromListN (Prim.length v) <$> traversed (Indexed f') (Prim.toList v)
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: ('Storable' a, 'Storable' b) => 'IndexedTraversal' 'Int' ('Storable.Vector' a) ('Storable.Vector' b) a b@
instance (Applicative f, Storable a, Storable b) => Each f (Storable.Vector a) (Storable.Vector b) a b where
  each f v = Storable.fromListN (Storable.length v) <$> traversed (Indexed f') (Storable.toList v)
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: ('Unbox' a, 'Unbox' b) => 'IndexedTraversal' 'Int' ('Unboxed.Vector' a) ('Unboxed.Vector' b) a b@
instance (Applicative f, Unbox a, Unbox b) => Each f (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each f v = Unboxed.fromListN (Unboxed.length v) <$> traversed (Indexed f') (Unboxed.toList v)
    where f' = Lens.indexed f
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' 'StrictT.Text' 'StrictT.Text' 'Char' 'Char'@
instance Applicative f =>  Each f StrictT.Text StrictT.Text Char Char where
  each = iso StrictT.unpack StrictT.pack . traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int64' 'LazyT.Text' 'LazyT.Text' 'Char' 'Char'@
instance Applicative f => Each f LazyT.Text LazyT.Text Char Char where
  each = iso LazyT.unpack LazyT.pack . traversed64
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int' 'StrictB.ByteString' 'StrictB.ByteString' 'Word8' 'Word8'@
instance Applicative f => Each f StrictB.ByteString StrictB.ByteString Word8 Word8 where
  each = iso StrictB.unpack StrictB.pack . traversed
  {-# INLINE each #-}

-- | @'each' :: 'IndexedTraversal' 'Int64' 'LazyB.ByteString' 'LazyB.ByteString' 'Word8' 'Word8'@
instance Applicative f => Each f LazyB.ByteString LazyB.ByteString Word8 Word8 where
  each = iso LazyB.unpack LazyB.pack . traversed64
  {-# INLINE each #-}

-- | @'each' :: 'Ix' i => 'IndexedTraversal' i ('Array' i a) ('Array' i b) a b@
instance (Applicative f, Ix i) => Each f (Array i a) (Array i b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> Lens.indexed f i a) (IArray.assocs arr)
  {-# INLINE each #-}

-- | @'each' :: ('Ix' i, 'IArray' 'UArray' a, 'IArray' 'UArray' b) => 'IndexedTraversal' i ('Array' i a) ('Array' i b) a b@
instance (Applicative f, Ix i, IArray UArray a, IArray UArray b) => Each f (UArray i a) (UArray i b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> Lens.indexed f i a) (IArray.assocs arr)
  {-# INLINE each #-}

-- | @'each' :: 'Control.Lens.IndexedSetter' i (i -> a) (i -> b) a b@
instance Settable f => Each f (i -> a) (i -> b) a b where
  each f g = pure (\i -> untaintedDot (Lens.indexed f i) (g i))
  {-# INLINE each #-}
