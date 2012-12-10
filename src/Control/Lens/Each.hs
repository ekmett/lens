{-# LANGUAGE TypeFamilies #-}
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
import Control.Lens.Traversal

-- | Extract 'each' element of a (potentially monomorphic) container.
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable f, s ~ f a, t ~ f b) => Traversal s t a b
  each = traverse

instance Each [a] [b] a b

instance (a ~ a', b ~ b') => Each (a,a') (b,b') a b where
  each = both

instance (a ~ a2, a ~ a3, b ~ b2, b ~ b3) => Each (a,a2,a3) (b,b2,b3) a b where
  each f (a,b,c) = (,,) <$> f a <*> f b <*> f c

instance (a ~ a2, a ~ a3, a ~ a4, b ~ b2, b ~ b3, b ~ b4) => Each (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each f (a,b,c,d) = (,,,) <$> f a <*> f b <*> f c <*> f d

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, b ~ b2, b ~ b3, b ~ b4, b ~ b5) => Each (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each f (a,b,c,d,e) = (,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6) => Each (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each f (a,b,c,d,e,g) = (,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7) => Each (a,a2,a3,a4,a5,a6,a7) (b,b2,b3,b4,b5,b6,b7) a b where
  each f (a,b,c,d,e,g,h) = (,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8) => Each (a,a2,a3,a4,a5,a6,a7,a8) (b,b2,b3,b4,b5,b6,b7,b8) a b where
  each f (a,b,c,d,e,g,h,i) = (,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9) => Each (a,a2,a3,a4,a5,a6,a7,a8,a9) (b,b2,b3,b4,b5,b6,b7,b8,b9) a b where
  each f (a,b,c,d,e,g,h,i,j) = (,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j
