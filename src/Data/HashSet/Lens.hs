{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashSet.Lens
-- Copyright   :  (C) 2012 Edward Kmett
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

import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Setter
import Data.HashSet as HashSet
import Data.Hashable

-- $setup
-- >>> :m + Data.HashSet Control.Lens

-- | This 'Setter' can be used to change the type of a 'HashSet' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', but you can
-- manipulate it by reading using 'folded' and reindexing it via 'setmap'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: (Eq i, Hashable i, Eq j, Hashable j) => Setter (HashSet i) (HashSet j) i j
setmapped = sets HashSet.map
{-# INLINE setmapped #-}

-- | Construct a set from a 'Getter', 'Fold', 'Traversal', 'Lens' or 'Iso'.
--
-- >>> setOf (folded._2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
--
-- @
-- setOf :: 'Hashable' c         => 'Getter' a c           -> a -> 'HashSet' c
-- setOf :: ('Eq' c, 'Hashable' c) => 'Fold' a c             -> a -> 'HashSet' c
-- setOf :: 'Hashable' c         => 'Simple' 'Iso' a c       -> a -> 'HashSet' c
-- setOf :: 'Hashable' c         => 'Simple' 'Lens' a c      -> a -> 'HashSet' c
-- setOf :: ('Eq' c, 'Hashable' c) => 'Simple' 'Traversal' a c -> a -> 'HashSet' c
-- @
setOf :: Hashable c => Getting (HashSet c) a b c d -> a -> HashSet c
setOf l = runAccessor . l (Accessor . HashSet.singleton)
{-# INLINE setOf #-}
