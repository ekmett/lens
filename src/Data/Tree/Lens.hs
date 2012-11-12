{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MTPCs
--
----------------------------------------------------------------------------

module Data.Tree.Lens
  ( root
  , branches
  ) where

import Control.Lens
import Data.Functor
import Data.Tree

-- | A 'Lens' that focuses on the root of a 'Tree'.
--
-- >>> view root $ Node 42 []
-- 42
root :: Simple Lens (Tree a) a
root f (Node a as) = (`Node` as) <$> f a
{-# INLINE root #-}

-- | A 'Traversal' of the direct descendants of the root of a 'Tree'
-- indexed by its position in the list of children
--
-- @'toListOf' 'branches' ≡ 'subForest'@
branches :: SimpleIndexedTraversal Int (Tree a) (Tree a)
branches = index $ \ f (Node a as) -> Node a <$> itraverse f as
{-# INLINE branches #-}
