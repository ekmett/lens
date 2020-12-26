{-# LANGUAGE FlexibleContexts #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
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

import Prelude ()

import Control.Lens.Internal.Prelude
import Control.Lens
import Data.Tree

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Tree

-- | A 'Lens' that focuses on the root of a 'Tree'.
--
-- >>> view root $ Node 42 []
-- 42
root :: Lens' (Tree a) a
root f (Node a as) = (`Node` as) <$> f a
{-# INLINE root #-}

-- | A 'Lens' returning the direct descendants of the root of a 'Tree'
--
-- @'view' 'branches' ≡ 'subForest'@
branches :: Lens' (Tree a) [Tree a]
branches f (Node a as) = Node a <$> f as
{-# INLINE branches #-}
