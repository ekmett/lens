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
  , projecting
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
type Projection s t a b = forall k f. (Projective k t b, Applicative f) => k (a -> f b) (s -> f t)

-- | Used to provide overloading of projections.
class Projective k a d where
  projective :: (d -> a) -> (x -> y) -> k x y

instance Projective (->) a d where
  projective _ x = x

-- | A concrete 'Projection', suitable for storing in a container or extracting an embedding.
data Project s b x y = Project (b -> s) (x -> y)

-- | Compose projections.
stereo :: Projective k s a => Project t a y z -> Project s t x y -> k x z
stereo (Project g f) (Project i h) = projective (i.g) (f.h)

instance (s ~ s', b ~ b') => Projective (Project s b) s' b' where
  projective = Project

-- | Reflect a 'Projection'.
project :: Projective k s b => Overloaded (Project s b) f s s a b -> Overloaded k f s s a b
project (Project f g) = projective f g

-- | Turn a 'Projection' around to get an embedding
by :: Project s b (b -> Identity b) (s -> Identity s) -> Getter b s
by (Project g _) = to g

-- | Build a 'Projection'
projection :: (a -> s) -> (s -> Maybe a) -> Projection s s a a
projection bt sma = projective bt (\afb a -> maybe (pure a) (fmap bt . afb) (sma a))

projecting :: (b -> t) -> (forall f. Applicative f => (a -> f b) -> s -> f t) -> Projection s t a b
projecting bt k = projective bt k

-- | Convert an 'Iso' to a 'Projection'.
--
-- Ideally we would be able to use an 'Iso' directly as a 'Projection', but this opens a can of worms.
mirror :: Projective k s a => Simple Iso s a -> Simple Projection s a
mirror l = projection (^.from l) (\a -> Just (a^.l))

-- | @type 'SimpleProjection' = 'Simple' 'Projection'@
type SimpleProjection s a = Projection s s a a
