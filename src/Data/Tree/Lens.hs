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
  , children
  ) where

import Control.Lens
import Data.Functor
import Data.List.Lens
import Data.Tree

-- | A 'Lens' that focuses on the root of a 'Tree'.
root :: Simple Lens (Tree a) a
root f (Node a as) = (`Node` as) <$> f a
{-# INLINE root #-}

-- | A 'Traversal' of the direct descendants of the root of a 'Tree'
-- indexed by its position in the list of children
children :: SimpleIndexedTraversal Int (Tree a) (Tree a)
children = index $ \ f (Node a as) -> Node a <$> withIndex traverseList f as
{-# INLINE children #-}
