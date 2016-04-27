{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Each
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.Each
  (
  -- * Each
    Each(..)
  ) where

import Control.Lens.Traversal
import Control.Lens.Internal.ByteString
import Data.Array.Unboxed as Unboxed
import Data.Array.IArray as IArray
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Complex
import Data.Functor.Identity
import Data.HashMap.Lazy as HashMap
import Data.IntMap as IntMap
import Data.List.NonEmpty
import Data.Map as Map
import Data.Sequence as Seq
import Data.Text.Lens (text)
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Tree as Tree
import Data.Vector.Generic.Lens (vectorTraverse)
import qualified Data.Vector as Vector
import qualified Data.Vector.Primitive as Prim
import Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable as Storable
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Unboxed as Unboxed
import Data.Vector.Unboxed (Unbox)
import Data.Word

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

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
-- >>> ("hello","world") & each.each %~ Char.toUpper
-- ("HELLO","WORLD")
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a) (b,b) a b@
instance (a~a', b~b') => Each (a,a') (b,b') a b where
  each f ~(a,b) = (,) <$> f a <*> f b
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a) (b,b,b) a b@
instance (a~a2, a~a3, b~b2, b~b3) => Each (a,a2,a3) (b,b2,b3) a b where
  each f ~(a,b,c) = (,,) <$> f a <*> f b <*> f c
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a) (b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, b~b2, b~b3, b~b4) => Each (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each f ~(a,b,c,d) = (,,,) <$> f a <*> f b <*> f c <*> f d
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a) (b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, b~b2, b~b3, b~b4, b~b5) => Each (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each f ~(a,b,c,d,e) = (,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a,a) (b,b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, a~a6, b~b2, b~b3, b~b4, b~b5, b~b6) => Each (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each f ~(a,b,c,d,e,g) = (,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a,a,a) (b,b,b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7) => Each (a,a2,a3,a4,a5,a6,a7) (b,b2,b3,b4,b5,b6,b7) a b where
  each f ~(a,b,c,d,e,g,h) = (,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a,a,a,a) (b,b,b,b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8) => Each (a,a2,a3,a4,a5,a6,a7,a8) (b,b2,b3,b4,b5,b6,b7,b8) a b where
  each f ~(a,b,c,d,e,g,h,i) = (,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a,a,a,a,a) (b,b,b,b,b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, a~a9, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8, b~b9) => Each (a,a2,a3,a4,a5,a6,a7,a8,a9) (b,b2,b3,b4,b5,b6,b7,b8,b9) a b where
  each f ~(a,b,c,d,e,g,h,i,j) = (,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j
  {-# INLINE each #-}

-- | @'each' :: ('RealFloat' a, 'RealFloat' b) => 'Traversal' ('Complex' a) ('Complex' b) a b@
instance Each (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f a <*> f b
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' ('Map' c a) ('Map' c b) a b@
instance (c ~ d) => Each (Map c a) (Map d b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' ('Map' c a) ('Map' c b) a b@
instance Each (IntMap a) (IntMap b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' ('HashMap' c a) ('HashMap' c b) a b@
instance (c ~ d) => Each (HashMap c a) (HashMap d b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' [a] [b] a b@
instance Each [a] [b] a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (NonEmpty a) (NonEmpty b) a b@
instance Each (NonEmpty a) (NonEmpty b) a b

-- | @'each' :: 'Traversal' ('Identity' a) ('Identity' b) a b@
instance Each (Identity a) (Identity b) a b

-- | @'each' :: 'Traversal' ('Maybe' a) ('Maybe' b) a b@
instance Each (Maybe a) (Maybe b) a b

-- | @'each' :: 'Traversal' ('Seq' a) ('Seq' b) a b@
instance Each (Seq a) (Seq b) a b where
  each = traversed
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' ('Tree' a) ('Tree' b) a b@
instance Each (Tree a) (Tree b) a b

-- | @'each' :: 'Traversal' ('Vector.Vector' a) ('Vector.Vector' b) a b@
instance Each (Vector.Vector a) (Vector.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Prim' a, 'Prim' b) => 'Traversal' ('Prim.Vector' a) ('Prim.Vector' b) a b@
instance (Prim a, Prim b) => Each (Prim.Vector a) (Prim.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Storable' a, 'Storable' b) => 'Traversal' ('Storable.Vector' a) ('Storable.Vector' b) a b@
instance (Storable a, Storable b) => Each (Storable.Vector a) (Storable.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Unbox' a, 'Unbox' b) => 'Traversal' ('Unboxed.Vector' a) ('Unboxed.Vector' b) a b@
instance (Unbox a, Unbox b) => Each (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'StrictT.Text' 'StrictT.Text' 'Char' 'Char'@
instance (a ~ Char, b ~ Char) => Each StrictT.Text StrictT.Text a b where
  each = text
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'LazyT.Text' 'LazyT.Text' 'Char' 'Char'@
instance (a ~ Char, b ~ Char) => Each LazyT.Text LazyT.Text a b where
  each = text
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'StrictB.ByteString' 'StrictB.ByteString' 'Word8' 'Word8'@
instance (a ~ Word8, b ~ Word8) => Each StrictB.ByteString StrictB.ByteString a b where
  each = traversedStrictTree
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'LazyB.ByteString' 'LazyB.ByteString' 'Word8' 'Word8'@
instance (a ~ Word8, b ~ Word8) => Each LazyB.ByteString LazyB.ByteString a b where
  each = traversedLazy
  {-# INLINE each #-}

-- | @'each' :: 'Ix' i => 'Traversal' ('Array' i a) ('Array' i b) a b@
instance (Ix i, i ~ j) => Each (Array i a) (Array j b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f a) (IArray.assocs arr)
  {-# INLINE each #-}

-- | @'each' :: ('Ix' i, 'IArray' 'UArray' a, 'IArray' 'UArray' b) => 'Traversal' ('Array' i a) ('Array' i b) a b@
instance (Ix i, IArray UArray a, IArray UArray b, i ~ j) => Each (UArray i a) (UArray j b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f a) (IArray.assocs arr)
  {-# INLINE each #-}
