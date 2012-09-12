{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Projection
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.Projection
  ( Projection
  , Projective(..)
  , project
  , by
  , Project(..)
  , projection
  , stereo
  , mirror
  -- * Simple
  , SimpleProjection
  ) where

import Control.Applicative
import Control.Lens.Type
import Control.Lens.Getter
import Data.Functor.Identity
import Control.Lens.Iso

-- | A 'Projection' is a 'Traversal' that can also be turned around with 'by' to obtain a 'Getter'
type Projection a b c d = forall k f. (Projective k a d, Applicative f) => k (c -> f d) (a -> f a)

-- | Used to provide overloading of projections.
class Projective k a d where
  projective :: (d -> a) -> (x -> y) -> k x y

instance Projective (->) a d where
  projective _ x = x

-- | A concrete 'Projection', suitable for storing in a container or extracting an embedding.
data Project a d x y = Project (d -> a) (x -> y)

-- | Compose projections.
stereo :: Projective k a c => Project b c y z -> Project a b x y -> k x z
stereo (Project g f) (Project i h) = projective (i.g) (f.h)

instance (a ~ a', d ~ d') => Projective (Project a d) a' d' where
  projective = Project

-- | Reflect a 'Projection'.
project :: Projective k a d => Overloaded (Project a d) f a a c d -> Overloaded k f a a c d
project (Project f g) = projective f g

-- | Turn a 'Projection' around to get an embedding
by :: Project a d (d -> Identity d) (a -> Identity a) -> Getter d a
by (Project g _) = to g

-- | Build a 'Projection'
projection :: (d -> a) -> (a -> Maybe c) -> Projection a b c d
projection da amc = projective da (\cfd a -> maybe (pure a) (fmap da . cfd) (amc a))

-- | Convert an 'Iso' to a 'Projection'.
--
-- Ideally we would be able to use an 'Iso' directly as a 'Projection', but this opens a can of worms.
mirror :: Projective k a c => Simple Iso a c -> Simple Projection a c
mirror l = projection (^.from l) (\a -> Just (a^.l))

-- | @type 'SimpleProjection' = 'Simple' 'Projection'@
type SimpleProjection a b = Projection a a b b
