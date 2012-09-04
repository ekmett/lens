-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lens' or 'Traversal' can be used to take the role of 'Traversable' in
-- @Control.Parallel.Strategies@, enabling those combinators to work with
-- monomorphic containers.
----------------------------------------------------------------------------
module Control.Parallel.Strategies.Lens
  ( evalOf
  , parOf
  , after
  , throughout
  ) where

import Control.Lens
import Control.Parallel.Strategies

-- | Evaluate the targets of a 'Lens' or 'Traversal' into a data structure
-- according to the given 'Strategy'.
--
-- @
-- 'evalTraversable' = 'evalOf' 'traverse' = 'traverse'
-- 'evalOf' = 'id'
-- @
--
-- @
-- 'evalOf' :: 'Simple' 'Lens' a b -> 'Strategy' b -> 'Strategy' a
-- 'evalOf' :: 'Simple' 'Traversal' a b -> 'Strategy' b -> 'Strategy' a
-- 'evalOf' :: (b -> 'Eval' b) -> a -> 'Eval' a) -> 'Strategy' b -> 'Strategy' a
-- @
evalOf :: SimpleLensLike Eval a b -> Strategy b -> Strategy a
evalOf l = l
{-# INLINE evalOf #-}

-- | Evaluate the targets of a 'Lens' or 'Traversal' according into a
-- data structure according to a given 'Strategy' in parallel.
--
-- @'parTraversable' = 'parOf' 'traverse'@
--
-- @
-- 'parOf' :: 'Simple' 'Lens' a b -> 'Strategy' b -> 'Strategy' a
-- 'parOf' :: 'Simple' 'Traversal' a b -> 'Strategy' b -> 'Strategy' a
-- 'parOf' :: ((b -> 'Eval' b) -> a -> 'Eval' a) -> 'Strategy' b -> 'Strategy' a
-- @
parOf :: SimpleLensLike Eval a b -> Strategy b -> Strategy a
parOf l s = l (rparWith s)
{-# INLINE parOf #-}

-- | Transform a 'Lens', 'Fold', 'Getter', 'Setter' or 'Traversal' to
-- first evaluates its argument according to a given 'Strategy' /before/ proceeding.
--
-- @
-- 'after' 'rdeepseq' 'traverse' :: 'Traversable' t => 'Strategy' a -> 'Strategy' [a]
-- @
after :: Strategy a -> LensLike f a b c d -> LensLike f a b c d
after s l f = l f $| s
{-# INLINE after #-}

-- | Transform a 'Lens', 'Fold', 'Getter', 'Setter' or 'Traversal' to
-- evaluate its argument according to a given 'Strategy' /in parallel with/ evaluating.
--
-- @
-- 'throughout' 'rdeepseq' 'traverse' :: 'Traversable' t => 'Strategy' a -> 'Strategy' [a]
-- @
throughout :: Strategy a -> LensLike f a b c d -> LensLike f a b c d
throughout s l f = l f $|| s
{-# INLINE throughout #-}
