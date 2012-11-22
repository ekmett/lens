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
import Control.Lens.Classes

------------------------------------------------------------------------------
-- Indexed Getters
------------------------------------------------------------------------------

-- | Every 'IndexedGetter' is a valid 'Control.Lens.IndexedFold.IndexedFold' and 'Getter'.
type IndexedGetter i s a = forall k f. (Indexed i k, Gettable f) => k (a -> f a) (s -> f s)

-- | Used to consume an 'Control.Lens.IndexedFold.IndexedFold'.
type IndexedGetting i m s t a b = Index i (a -> Accessor m b) (s -> Accessor m t)

-- | Useful for storage.
newtype ReifiedIndexedGetter i s a = ReifyIndexedGetter { reflectIndexedGetter :: IndexedGetter i s a }
