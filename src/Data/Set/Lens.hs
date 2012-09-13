{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Set.Lens
  ( setmapped
  , setOf
  ) where

import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Setter
import Data.Set as Set

-- $setup
-- >>> :m + Data.Set.Lens Control.Lens

-- | This 'Setter' can be used to change the type of a 'Set' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Control.Lens.Traversal.Traversal' for a 'Set', but you can
-- manipulate it by reading using 'folded' and reindexing it via 'setmap'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: (Ord i, Ord j) => Setter (Set i) (Set j) i j
setmapped = sets Set.map
{-# INLINE setmapped #-}

-- | Construct a set from a 'Getter', 'Control.Lens.Fold.Fold', 'Control.Lens.Traversal.Traversal', 'Control.Lens.Type.Lens' or 'Control.Lens.Iso.Iso'.
--
-- >>> setOf (folded._2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
--
-- @
-- setOf ::          'Getter' a c           -> a -> 'Set' c
-- setOf :: 'Ord' c => 'Control.Lens.Fold.Fold' a c             -> a -> 'Set' c
-- setOf ::          'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Set' c
-- setOf ::          'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c      -> a -> 'Set' c
-- setOf :: 'Ord' c => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Set' c
-- @
setOf :: Getting (Set c) a b c d -> a -> Set c
setOf l = runAccessor . l (Accessor . Set.singleton)
{-# INLINE setOf #-}
