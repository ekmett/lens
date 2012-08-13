-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntSet.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.IntSet.Lens
  ( contains
  , members
  , setmapped
  , setOf
  ) where

import Control.Applicative
import Control.Lens
import Data.IntSet as IntSet

-- | This 'Lens' can be used to read, write or delete a member of an 'IntSet'
--
-- > ghci> contains 3 +~ False $ fromList [1,2,3,4]
-- > fromList [1,2,4]
--
-- @contains :: 'Functor' f => 'Int' -> ('Bool' -> f 'Bool') -> 'IntSet' -> f 'IntSet'@
contains :: Int -> Simple Lens IntSet Bool
contains k f s = go <$> f (IntSet.member k s) where
  go False = IntSet.delete k s
  go True  = IntSet.insert k s
{-# INLINE contains #-}

-- | IntSet isn't Foldable, but this 'Fold' can be used to access the members of an 'IntSet'.
--
-- >>> sumOf members $ setOf folded [1,2,3,4]
-- 10
members :: Fold IntSet Int
members = folding IntSet.toAscList

-- | This 'Setter' can be used to change the contents of an 'IntSet' by mapping
-- the elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', because the number of
-- elements might change but you can manipulate it by reading using 'folded' and
-- reindexing it via 'setmap'.
--
-- >>> under setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: Simple Setter IntSet Int
setmapped = sets IntSet.map

-- | Construct an 'IntSet' from a 'Getter', 'Fold', 'Traversal', 'Lens' or 'Iso'.
--
-- >>> :m + Data.IntSet.Lens Control.Lens
-- >>> setOf (folded._2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
--
-- @
-- setOf :: 'Getter' a 'Int'           -> a -> 'IntSet'
-- setOf :: 'Fold' a 'Int'             -> a -> 'IntSet'
-- setOf :: 'Simple' 'Iso' a 'Int'       -> a -> 'IntSet'
-- setOf :: 'Simple' 'Lens' a 'Int'      -> a -> 'IntSet'
-- setOf :: 'Simple' 'Traversal' a 'Int' -> a -> 'IntSet'
-- @
setOf :: Getting IntSet a Int -> a -> IntSet
setOf l = runAccessor . l (Accessor . IntSet.singleton)
