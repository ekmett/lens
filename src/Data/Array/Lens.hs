{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, Rank2Types, LiberalTypeSynonyms
--
----------------------------------------------------------------------------
module Data.Array.Lens
  (
  -- * Setters
    ixmapped
  ) where

import Control.Lens
import Data.Array.IArray hiding (index)

-- | This 'setter' can be used to derive a new 'IArray' from an old 'IAarray' by
-- applying a function to each of the indices to look it up in the old 'IArray'.
--
-- This is a /contravariant/ 'Setter'.
--
-- @
-- 'ixmap' ≡ 'over' '.' 'ixmapped'
-- 'ixmapped' ≡ 'setting' '.' 'ixmap'
-- 'over' ('ixmapped' b) f arr '!' i ≡ arr '!' f i
-- 'bounds' ('over' ('ixmapped' b) f arr) ≡ b
-- @
ixmapped :: (IArray a e, Ix i, Ix j) => (i,i) -> IndexPreservingSetter (a j e) (a i e) i j
ixmapped = setting . ixmap
{-# INLINE ixmapped #-}
