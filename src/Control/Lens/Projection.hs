{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , stereo
  , mirror
  -- * Common projections
  , _left
  , _right
  -- * Simple
  , SimpleProjection
  ) where

import Control.Applicative
import Control.Lens.Type
import Control.Lens.Getter
import Data.Functor.Identity
import Control.Lens.Iso
import Control.Lens.Traversal

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
projecting :: (b -> t) -> Traversal s t a b -> Projection s t a b
projecting bt k = projective bt k

-- | Convert an 'Iso' to a 'Projection'.
--
-- Ideally we would be able to use an 'Iso' directly as a 'Projection', but this opens a can of worms.
mirror :: Projective k s a => Simple Iso s a -> Simple Projection s a
mirror l = projecting (review l) l

-- | @type 'SimpleProjection' = 'Simple' 'Projection'@
type SimpleProjection s a = Projection s s a a

-- | A traversal for tweaking the left-hand value of an 'Either':
--
-- >>> over _left (+1) (Left 2)
-- Left 3
--
-- >>> over _left (+1) (Right 2)
-- Right 2
--
-- >>> Right 42 ^._left :: String
-- ""
--
-- >>> Left "hello" ^._left
-- "hello"
--
-- @_left :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ('Either' b c)@
_left :: Projection (Either a c) (Either b c) a b
_left = projecting Left $ \ f e -> case e of
  Left a  -> Left <$> f a
  Right c -> pure $ Right c
{-# INLINE _left #-}

-- | traverse the right-hand value of an 'Either':
--
-- @'_right' ≡ 'Data.Traversable.traverse'@
--
-- Unfortunately the instance for
-- @'Data.Traversable.Traversable' ('Either' c)@ is still missing from base,
-- so this can't just be 'Data.Traversable.traverse'
--
-- >>> over _right (+1) (Left 2)
-- Left 2
--
-- >>> over _right (+1) (Right 2)
-- Right 3
--
-- >>> Right "hello" ^._right
-- "hello"
--
-- >>> Left "hello" ^._right :: [Double]
-- []
--
-- @_right :: 'Applicative' f => (a -> f b) -> 'Either' c a -> f ('Either' c a)@
_right :: Projection (Either c a) (Either c b) a b
_right = projecting Right $ \f e -> case e of
  Left c -> pure $ Left c
  Right a -> Right <$> f a
{-# INLINE _right #-}
