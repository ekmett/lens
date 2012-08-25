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
  ( contains
  , setmapped
  , setOf
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Lens.Type
import Data.HashSet as HashSet
import Data.Hashable

-- | This 'Lens' can be used to read, write or delete a member of a 'HashSet'
--
-- >>> :m + Data.HashSet Data.HashSet.Lens Control.Lens
-- >>> contains 3 .~ False $ HashSet.fromList [1,2,3,4]
-- fromList [1,2,4]
contains :: (Eq k, Hashable k) => k -> Simple Lens (HashSet k) Bool
contains k f s = go <$> f (HashSet.member k s) where
  go False = HashSet.delete k s
  go True  = HashSet.insert k s
{-# INLINE contains #-}

-- | This 'Setter' can be used to change the type of a 'HashSet' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', but you can
-- manipulate it by reading using 'folded' and reindexing it via 'setmap'.
--
-- >>> :m + Data.HashSet Data.HashSet.Lens Control.Lens
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: (Eq i, Hashable i, Eq j, Hashable j) => Setter (HashSet i) (HashSet j) i j
setmapped = sets HashSet.map

-- | Construct a set from a 'Getter', 'Fold', 'Traversal', 'Lens' or 'Iso'.
--
-- >>> :m + Data.Set.Lens Control.Lens
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
setOf :: Hashable c => Getting (HashSet c) a c -> a -> HashSet c
setOf l = runAccessor . l (Accessor . HashSet.singleton)
