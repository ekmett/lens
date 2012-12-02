-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Combinators
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-------------------------------------------------------------------------------
module Control.Lens.Combinators
  ( (<$!>), (<$!), (<&>)
  , (>>&), (>&>)
  ) where

import Control.Monad (liftM)
import Data.Functor

infixl 4 <$!>, <$!, <&>
infixl 1 >>&, >&>

-- | A strict version of ('Data.Functor.<$>') for monads.
--
-- >>> (+1) <$!> [1,2,3,4]
-- [2,3,4,5]
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  a <- m
  return $! f a
{-# INLINE (<$!>) #-}

-- | A strict version of ('Data.Functor.<$') for monads.
--
-- >>> () <$! [1,2,3,4]
-- [(),(),(),()]
(<$!) :: Monad m => b -> m a -> m b
b <$! m = do
  _ <- m
  return $! b
{-# INLINE (<$!) #-}

-- | Infix flipped fmap.
--
-- @('<&>') = 'flip' 'fmap'@
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

-- | Provided by analogy to @('>>=')@
--
-- @('>>&') ≡ 'flip' 'liftM'@
(>>&) :: Monad m => m a -> (a -> b) -> m b
(>>&) = flip liftM
{-# INLINE (>>&) #-}

-- | Provided by analogy to ('>=>')
--
-- Compose a 'Kleisli' action with a normal function.
(>&>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
amb >&> bc = liftM bc . amb
{-# INLINE (>&>) #-}
