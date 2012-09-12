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
  , composeProjection
  ) where

import Control.Applicative
import Control.Lens.Type
import Control.Lens.Getter

-- | A Projection is a 'Traversal' that can also be turned around with 'by' to obtain a 'Getter'
type Projection a b c d = forall k f. (Projective k a d, Applicative f) => k (c -> f d) (a -> f a)

class Projective k a d where
  projective :: (d -> a) -> (x -> y) -> k x y

instance Projective (->) a d where
  projective _ x = x

-- | A reified projection
data Project a d x y = Project (d -> a) (x -> y)

-- | Compose two projections.
composeProjection :: Projective k a c => Project b c y z -> Project a b x y -> k x z
composeProjection (Project g f) (Project i h) = projective (i.g) (f.h)

instance (a ~ a', d ~ d') => Projective (Project a d) a' d' where
  projective = Project

-- | reflect a projection
project :: Projective k a d => Overloaded (Project a d) f a a c d -> Overloaded k f a a c d
project (Project f g) = projective f g

-- | Turn a projection around to get an embedding
by :: Project a d x y -> Getter d a
by (Project g _) = to g

-- | Build a projection
projection :: (d -> a) -> (a -> Maybe c) -> Projection a b c d
projection da amc = projective da (\cfd a -> maybe (pure a) (fmap da . cfd) (amc a))
