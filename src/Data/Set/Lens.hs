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
  ( contains
  , members
  ) where

import Control.Lens
import Data.Set as Set
import Data.Functor

-- | This 'Lens' can be used to read, write or delete a member of a 'Set'
--
-- > ghci> contains 3 +~ False $ Set.fromList [1,2,3,4]
-- > fromList [1,2,4]
--
-- > contains :: Ord k => k -> (Bool -> f Bool) -> Set k -> f (Set k)
contains :: Ord k => k -> Simple Lens (Set k) Bool
contains k f s = go <$> f (Set.member k s) where
  go False = Set.delete k s
  go True  = Set.insert k s
{-# INLINE contains #-}

-- | This 'Setter' can be used to change the type of a 'Set' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', but you can
-- manipulate it by reading using 'folded' and reindexing it via 'setmap'.
--
-- > ghci> adjust members (+1) (fromList [1,2,3,4]
-- > fromList [2,3,4,5]
members :: (Ord i, Ord j) => Setter (Set i) (Set j) i j
members = sets Set.map
