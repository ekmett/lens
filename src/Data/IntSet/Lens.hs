module Data.IntSet.Lens
  ( contains
  ) where

import Control.Lens
import Data.Functor
import Data.IntSet as IntSet

-- | This 'Lens' can be used to read, write or delete a member of an 'IntSet'
--
-- > ghci> contains 3 +~ False $ fromList [1,2,3,4]
-- > fromList [1,2,4]
--
-- > contains :: Int -> (Bool -> f Bool) -> IntSet -> f IntSet
contains :: Int -> Simple Lens IntSet Bool
contains k f s = go <$> f (IntSet.member k s) where
  go False = IntSet.delete k s
  go True  = IntSet.insert k s
{-# INLINE contains #-}
