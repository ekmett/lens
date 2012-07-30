module Data.Set.Lens
  ( contains
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
