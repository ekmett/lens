{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntSet.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.IntSet.Lens
  ( members
  , setmapped
  , setOf
  ) where

import Control.Lens
import Data.IntSet as IntSet

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | IntSet isn't Foldable, but this 'Fold' can be used to access the members of an 'IntSet'.
--
-- >>> sumOf members $ setOf folded [1,2,3,4]
-- 10
members :: Fold IntSet Int
members = folding IntSet.toAscList
{-# INLINE members #-}

-- | This 'Setter' can be used to change the contents of an 'IntSet' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', because the number of
-- elements might change but you can manipulate it by reading using 'folded' and
-- reindexing it via 'setmapped'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: IndexPreservingSetter' IntSet Int
setmapped = setting IntSet.map
{-# INLINE setmapped #-}

-- | Construct an 'IntSet' from a 'Getter', 'Fold', 'Traversal', 'Lens' or 'Iso'.
--
-- >>> setOf folded [1,2,3,4]
-- fromList [1,2,3,4]
--
-- >>> setOf (folded._2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
--
-- @
-- 'setOf' :: 'Getter' s 'Int'     -> s -> 'IntSet'
-- 'setOf' :: 'Fold' s 'Int'       -> s -> 'IntSet'
-- 'setOf' :: 'Iso'' s 'Int'       -> s -> 'IntSet'
-- 'setOf' :: 'Lens'' s 'Int'      -> s -> 'IntSet'
-- 'setOf' :: 'Traversal'' s 'Int' -> s -> 'IntSet'
-- @
setOf :: Getting IntSet s Int -> s -> IntSet
setOf l = views l IntSet.singleton
{-# INLINE setOf #-}
