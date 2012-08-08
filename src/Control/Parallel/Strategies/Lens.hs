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
  , meanwhile
  ) where

import Control.Lens
import Control.Parallel.Strategies

-- | Evaluate the targets of a 'Lens' or 'Traversal' into a data structure
-- according to the given strategy.
--
-- @
-- 'evalTraversable' = 'evalTraversal' 'traverse' = 'traverse'
-- 'evalTraversal' = 'id'
-- @
--
-- @
-- evalTraversal :: 'Simple' 'Lens' a b -> 'Strategy' b -> 'Strategy' a
-- evalTraversal :: 'Simple' 'Traversal' a b -> 'Strategy' b -> 'Strategy' a
-- evalTraversal :: (b -> 'Eval' b) -> a -> 'Eval' a) -> 'Strategy' b -> 'Strategy' a
-- @
evalOf :: LensLike Eval a a b b -> Strategy b -> Strategy a
evalOf l = l

-- | Evaluate the targets of a 'Lens' or 'Traversal' according into a
-- data structure according to a given 'Strategy' in parallel.
--
-- @'parTraversable' = 'parTraversal' 'traverse'@
--
-- @
-- parTraversal :: 'Simple' 'Lens' a b -> 'Strategy' b -> 'Strategy' a
-- parTraversal :: 'Simple' 'Traversal' a b -> 'Strategy' b -> 'Strategy' a
-- parTraversal :: ((b -> 'Eval' b) -> a -> 'Eval' a) -> 'Strategy' b -> 'Strategy' a
-- @
parOf :: LensLike Eval a a b b -> Strategy b -> Strategy a
parOf l s = l (rparWith s)

-- | Transform a 'Lens', 'Fold', 'Getter', 'Setter' or 'Traversal' to
-- first evaluates its argument according to a given strategy /before/ proceeding.
--
-- @
-- 'after' 'rdeepseq' 'traverse' :: 'Traversable' t => 'Strategy' a -> 'Strategy' [a]
-- @
after :: Strategy a -> LensLike f a b c d -> LensLike f a b c d
after s l f = l f $| s

-- | Transform a 'Lens', 'Fold', 'Getter', 'Setter' or 'Traversal' to
-- evaluate its argument according to a given strategy /in parallel with/ evaluating.
--
-- @
-- 'meanwhile' 'rdeepseq' 'traverse' :: 'Traversable' t => 'Strategy' a -> 'Strategy' [a]
-- @
meanwhile :: Strategy a -> LensLike f a b c d -> LensLike f a b c d
meanwhile s l f = l f $|| s
