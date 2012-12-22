{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
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
import Control.Lens.Setter
import Data.HashSet as HashSet
import Data.Hashable

-- $setup
-- >>> :m + Data.HashSet Control.Lens

-- | This 'Setter' can be used to change the type of a 'HashSet' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Control.Lens.Traversal.Traversal' for a 'Set', but you can
-- manipulate it by reading using 'folded' and reindexing it via 'setmapped'.
--
setmapped :: (Eq i, Hashable i, Eq j, Hashable j) => Setter (HashSet i) (HashSet j) i j
setmapped = sets HashSet.map
{-# INLINE setmapped #-}

-- | Construct a set from a 'Getter', 'Control.Lens.Fold.Fold', 'Control.Lens.Traversal.Traversal', 'Control.Lens.Lens.Lens' or 'Control.Lens.Iso.Iso'.
--
-- @
-- 'setOf' :: 'Hashable' a         => 'Getter' s a           -> s -> 'HashSet' a
-- 'setOf' :: ('Eq' a, 'Hashable' a) => 'Control.Lens.Fold.Fold' s a             -> s -> 'HashSet' a
-- 'setOf' :: 'Hashable' a         => 'Control.Lens.Lens.Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'HashSet' a
-- 'setOf' :: 'Hashable' a         => 'Control.Lens.Lens.Simple' 'Control.Lens.Lens.Lens' s a      -> s -> 'HashSet' a
-- 'setOf' :: ('Eq' a, 'Hashable' a) => 'Control.Lens.Lens.Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'HashSet' a
-- @
setOf :: Hashable a => Getting (HashSet a) s t a b -> s -> HashSet a
setOf l = views l HashSet.singleton
{-# INLINE setOf #-}
