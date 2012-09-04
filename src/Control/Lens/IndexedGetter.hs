{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedGetter
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  rank 2 types, MPTCs
--
----------------------------------------------------------------------------
module Control.Lens.IndexedGetter
  (
  -- * Indexed Folds
    IndexedGetter
  , IndexedGetting
  , ReifiedIndexedGetter(..)
  ) where

import Control.Lens.Indexed
import Control.Lens.Internal

------------------------------------------------------------------------------
-- Indexed Getters
------------------------------------------------------------------------------

-- | Every 'IndexedGetter' is a valid 'Control.Lens.IndexedFold.IndexedFold' and 'Getter'.
type IndexedGetter i a c = forall k f. (Indexed i k, Gettable f) => k (c -> f c) (a -> f a)

-- | Used to consume an 'Control.Lens.IndexedFold.IndexedFold'.
type IndexedGetting i m a b c d = Index i (c -> Accessor m d) (a -> Accessor m b)

-- | Useful for storage.
newtype ReifiedIndexedGetter i a c = ReifyIndexedGetter { reflectIndexedGetter :: IndexedGetter i a c }
