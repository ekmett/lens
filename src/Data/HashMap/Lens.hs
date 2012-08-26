{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.HashMap.Lens
  ( traverseMap
  ) where

import Control.Lens.Indexed
import Control.Lens.IndexedTraversal
import Data.HashMap.Lazy as HashMap

-- | Traversal of a 'HashMap' indexed by the key.
--
-- @'traverseMap' = 'index' 'traverseWithKey'@
traverseMap :: IndexedTraversal k (HashMap k v) (HashMap k v') v v'
traverseMap = index traverseWithKey
{-# INLINE traverseMap #-}
