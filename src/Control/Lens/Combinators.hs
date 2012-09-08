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
  ( (|>)
  , (<$!>), (<$!)
  ) where

infixr 4 <$!>, <$!
infixl 1 |>

-- | Passes the result of the left side to the function on the right side (forward pipe operator).
--
-- This is the flipped version of ('$'), which is more common in languages like F# where it is needed
-- for inference. Here it is supplied for notational convenience and given a precedence that allows it
-- to be nested inside uses of ('$').
--
-- >>> "hello" |> length |> succ
-- 6
(|>) :: a -> (a -> b) -> b
a |> f = f a
{-# INLINE (|>) #-}

-- | A strict version of ('Data.Functor.<$>') for monads.
--
-- >>> (+1) <$!> [1,2,3,4]
-- [1,2,3,4]
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
