{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashSet.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.HashSet.Lens
  ( setmapped
  , setOf
  ) where

import Control.Lens.Getter (Getting, views)
import Control.Lens.Setter (setting)
import Control.Lens.Type
import Data.HashSet as HashSet
import Data.Hashable

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | This 'Setter' can be used to change the type of a 'HashSet' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', but you can
-- manipulate it by reading using 'Control.Lens.Fold.folded' and reindexing it via 'setmapped'.
setmapped :: (Eq j, Hashable j) => IndexPreservingSetter (HashSet i) (HashSet j) i j
setmapped = setting HashSet.map
{-# INLINE setmapped #-}

-- | Construct a set from a 'Getter', 'Control.Lens.Fold.Fold', 'Control.Lens.Traversal.Traversal', 'Control.Lens.Lens.Lens' or 'Control.Lens.Iso.Iso'.
--
-- @
-- 'setOf' :: 'Hashable' a         => 'Getter' s a     -> s -> 'HashSet' a
-- 'setOf' :: ('Eq' a, 'Hashable' a) => 'Fold' s a       -> s -> 'HashSet' a
-- 'setOf' :: 'Hashable' a         => 'Iso'' s a       -> s -> 'HashSet' a
-- 'setOf' :: 'Hashable' a         => 'Lens'' s a      -> s -> 'HashSet' a
-- 'setOf' :: ('Eq' a, 'Hashable' a) => 'Traversal'' s a -> s -> 'HashSet' a
-- @
setOf :: Hashable a => Getting (HashSet a) s a -> s -> HashSet a
setOf l = views l HashSet.singleton
{-# INLINE setOf #-}
