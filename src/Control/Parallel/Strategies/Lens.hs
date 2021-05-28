{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
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
-- 'evalOf' :: 'Lens'' s a -> 'Strategy' a -> 'Strategy' s
-- 'evalOf' :: 'Traversal'' s a -> 'Strategy' a -> 'Strategy' s
-- 'evalOf' :: (a -> 'Eval' a) -> s -> 'Eval' s) -> 'Strategy' a -> 'Strategy' s
-- @
evalOf :: LensLike' Eval s a -> Strategy a -> Strategy s
evalOf l = l
{-# INLINE evalOf #-}

-- | Evaluate the targets of a 'Lens' or 'Traversal' according into a
-- data structure according to a given 'Strategy' in parallel.
--
-- @'parTraversable' = 'parOf' 'traverse'@
--
-- @
-- 'parOf' :: 'Lens'' s a -> 'Strategy' a -> 'Strategy' s
-- 'parOf' :: 'Traversal'' s a -> 'Strategy' a -> 'Strategy' s
-- 'parOf' :: ((a -> 'Eval' a) -> s -> 'Eval' s) -> 'Strategy' a -> 'Strategy' s
-- @
parOf :: LensLike' Eval s a -> Strategy a -> Strategy s
parOf l s = l (rparWith s)
{-# INLINE parOf #-}

-- | Transform a 'Lens', 'Fold', 'Getter', 'Setter' or 'Traversal' to
-- first evaluates its argument according to a given 'Strategy' /before/ proceeding.
--
-- @
-- 'after' 'rdeepseq' 'traverse' :: 'Traversable' t => 'Strategy' a -> 'Strategy' [a]
-- @
after :: Strategy s -> LensLike f s t a b -> LensLike f s t a b
after s l f = l f $| s
{-# INLINE after #-}

-- | Transform a 'Lens', 'Fold', 'Getter', 'Setter' or 'Traversal' to
-- evaluate its argument according to a given 'Strategy' /in parallel with/ evaluating.
--
-- @
-- 'throughout' 'rdeepseq' 'traverse' :: 'Traversable' t => 'Strategy' a -> 'Strategy' [a]
-- @
throughout :: Strategy s -> LensLike f s t a b -> LensLike f s t a b
throughout s l f = l f $|| s
{-# INLINE throughout #-}
