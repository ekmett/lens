{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import Control.Lens.Indexed
import Control.Lens.IndexedTraversal
import Control.Lens.Traversal
import Control.Lens.Iso
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Word

-- | Extract 'each' element of a (potentially monomorphic) container.
class Each i s t a b | s -> i a, t -> i b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable f, s ~ f a, t ~ f b) => IndexedTraversal Int s t a b
  each = traversed

instance (a ~ a', b ~ b') => Each Int (a,a') (b,b') a b where
  each = indexed $ \ f ~(a,b) -> (,) <$> f (0 :: Int) a <*> f 1 b

instance (a ~ a2, a ~ a3, b ~ b2, b ~ b3) => Each Int (a,a2,a3) (b,b2,b3) a b where
  each = indexed $ \ f ~(a,b,c) -> (,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c

instance (a ~ a2, a ~ a3, a ~ a4, b ~ b2, b ~ b3, b ~ b4) => Each Int (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each = indexed $ \ f ~(a,b,c,d) -> (,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, b ~ b2, b ~ b3, b ~ b4, b ~ b5) => Each Int (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each = indexed $ \ f ~(a,b,c,d,e) -> (,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6) => Each Int (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each = indexed $ \ f ~(a,b,c,d,e,g) -> (,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7) => Each Int (a,a2,a3,a4,a5,a6,a7) (b,b2,b3,b4,b5,b6,b7) a b where
  each = indexed $ \ f ~(a,b,c,d,e,g,h) -> (,,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g <*> f 6 h

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8) => Each Int (a,a2,a3,a4,a5,a6,a7,a8) (b,b2,b3,b4,b5,b6,b7,b8) a b where
  each = indexed $ \ f ~(a,b,c,d,e,g,h,i) -> (,,,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g <*> f 6 h <*> f 7 i

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9) => Each Int (a,a2,a3,a4,a5,a6,a7,a8,a9) (b,b2,b3,b4,b5,b6,b7,b8,b9) a b where
  each = indexed $ \ f ~(a,b,c,d,e,g,h,i,j) -> (,,,,,,,,) <$> f (0 :: Int) a <*> f 1 b <*> f 2 c <*> f 3 d <*> f 4 e <*> f 5 g <*> f 6 h <*> f 7 i <*> f 8 j

instance Each Int [a] [b] a b

instance Each Int StrictT.Text StrictT.Text Char Char where
  each = iso StrictT.unpack StrictT.pack .> traversed

instance Each Int LazyT.Text LazyT.Text Char Char where
  each = iso LazyT.unpack LazyT.pack .> traversed

instance Each Int StrictB.ByteString StrictB.ByteString Word8 Word8 where
  each = iso StrictB.unpack StrictB.pack .> traversed

instance Each Int LazyB.ByteString LazyB.ByteString Word8 Word8 where
  each = iso LazyB.unpack LazyB.pack .> traversed
