{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Multi
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Note: 'traverse' is a 'MultiLensFamily'
----------------------------------------------------------------------------
module Control.Lens.Multi
  (
  -- * Lenses
    MultiLens
  , MultiLensFamily

  -- * Common lenses
  , constML
  , mapML
  , intMapML
  , headML
  , tailML
  , leftML
  , elementML
  ) where

import Control.Applicative
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Traversable

type MultiLens a b          = forall f. Applicative f => (b -> f b) -> a -> f a
type MultiLensFamily a b c d = forall f. Applicative f => (c -> f d) -> a -> f b

constML :: Applicative f => (a -> f a) -> b -> f b
constML = const pure

headML :: Applicative f => (a -> f a) -> [a] -> f [a]
headML _ [] = pure []
headML f (a:as) = (:as) <$> f a
{-# INLINE headML #-}

tailML :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
tailML _ [] = pure []
tailML f (a:as) = (a:) <$> f as
{-# INLINE tailML #-}

leftML :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
leftML f (Left a)  = Left <$> f a
leftML _ (Right c) = pure $ Right c
{-# INLINE leftML #-}

mapML :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
mapML k f m = case Map.lookup k m of
  Nothing -> pure m
  Just v -> (\v' -> Map.insert k v' m) <$> f v
{-# INLINE mapML #-}

intMapML :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
intMapML k f m = case IntMap.lookup k m of
  Nothing -> pure m
  Just v -> (\v' -> IntMap.insert k v' m) <$> f v
{-# INLINE intMapML #-}

newtype SA f a = SA { runSA :: Int -> (f a, Int) }

instance Functor f => Functor (SA f) where
  fmap f (SA m) = SA $ \i -> case m i of
    (fa, j) -> (fmap f fa, j)

instance Applicative f => Applicative (SA f) where
  pure a = SA (\i -> (pure a, i))
  SA mf <*> SA ma = SA $ \i -> case mf i of
    (ff, j) -> case ma j of
       (fa, k) -> (ff <*> fa, k)

elementML :: (Applicative f, Traversable t) => Int -> (a -> f a) -> t a -> f (t a)
elementML j f ta = fst (runSA (traverse go ta) 0) where
  go a = SA $ \i -> (if i == j then f a else pure a, i + 1)
