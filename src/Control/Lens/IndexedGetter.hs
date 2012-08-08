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
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.Internal
import Control.Monad
import Data.Monoid

------------------------------------------------------------------------------
-- Indexed Getters
------------------------------------------------------------------------------

-- | Every 'IndexedGetter' is a valid 'IndexedFold' and 'Getter'.
type IndexedGetter i a c = forall k f b d. (Indexed i k, Gettable f) => k (c -> f d) (a -> f b)

-- | Used to consume an 'IndexedFold'.
type IndexedGetting i m a b c d = Index i (c -> Accessor m d) (a -> Accessor m b)
