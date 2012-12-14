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
  ( (<$!>), (<$!), (<&>), (??)
  ) where

import Data.Functor ((<$>))

-- $setup
-- >>> import Control.Lens

infixl 4 <$!>, <$!
infixl 1 <&>, ??

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

-- | This is convenient to 'flip' argument order of composite functions
--
-- >>> over _2 ?? ("hello","world") $ length
-- ("hello",5)
--
-- >>> over ?? length ?? ("hello","world") $ _2
-- ("hello",5)
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($a) fab
