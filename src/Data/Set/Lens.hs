{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

{-# LANGUAGE Trustworthy #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-trustworthy-safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
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

import Control.Lens.Getter ( Getting, views )
import Control.Lens.Setter ( setting )
import Control.Lens.Type
import Data.Set as Set


-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | This 'Setter' can be used to change the type of a 'Set' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', but you can
-- manipulate it by reading using 'Control.Lens.Fold.folded' and reindexing it via 'setmapped'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
#if MIN_VERSION_containers(0,5,2)
setmapped :: Ord j => IndexPreservingSetter (Set i) (Set j) i j
#else
setmapped :: (Ord i, Ord j) => IndexPreservingSetter (Set i) (Set j) i j
#endif
setmapped = setting Set.map
{-# INLINE setmapped #-}

-- | Construct a set from a 'Getter', 'Control.Lens.Fold.Fold', 'Control.Lens.Traversal.Traversal', 'Control.Lens.Lens.Lens' or 'Control.Lens.Iso.Iso'.
--
-- >>> setOf folded ["hello","world"]
-- fromList ["hello","world"]
--
-- >>> setOf (folded._2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
--
-- @
-- 'setOf' ::          'Getter' s a     -> s -> 'Set' a
-- 'setOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Set' a
-- 'setOf' ::          'Iso'' s a       -> s -> 'Set' a
-- 'setOf' ::          'Lens'' s a      -> s -> 'Set' a
-- 'setOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Set' a
-- @
setOf :: Getting (Set a) s a -> s -> Set a
setOf l = views l Set.singleton
{-# INLINE setOf #-}
