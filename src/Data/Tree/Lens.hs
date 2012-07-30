-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------

module Data.Tree.Lens
  ( root
  , children
  ) where

import Control.Lens
import Data.Functor
import Data.Tree

-- | A 'Lens' that focuses on the root of a 'Tree'.
root :: Simple Lens (Tree a) a
root f (Node a as) = (`Node` as) <$> f a

-- | A 'Traversal' of the direct descendants of the root of a 'Tree'.
children :: Simple Traversal (Tree a) (Tree a)
children f (Node a as) = Node a <$> traverse f as
