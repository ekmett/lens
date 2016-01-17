-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Seq.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Fold' can be used to take the role of 'Foldable' in @Control.Seq@.
----------------------------------------------------------------------------
module Control.Seq.Lens
  ( seqOf
  ) where

import Control.Lens
import Control.Seq
import Data.Monoid

-- | Evaluate the elements targeted by a 'Lens', 'Traversal', 'Iso',
-- 'Getter' or 'Fold' according to the given strategy.
--
-- @'seqFoldable' = 'seqOf' 'folded'@
seqOf :: Getting (Endo [a]) s a -> Strategy a -> Strategy s
seqOf l s = seqList s . toListOf l
{-# INLINE seqOf #-}
