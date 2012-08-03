{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pair.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Lenses for working with products.
--
-- Due to their ubiquity, '_1' and '_2' are defined in @Control.Lens@.
----------------------------------------------------------------------------
module Data.Pair.Lens
  ( both
  , value
  ) where

import Control.Applicative
import Control.Lens

-- | Traverse both parts of a tuple with matching types.
both :: Traversal (a,a) (b,b) a b
both f (a,a') = (,) <$> f a <*> f a'
{-# INLINE both #-}

-- | This provides a 'Traversal' that checks a predicate on a key before
-- allowing you to traverse into a value.
value :: (k -> Bool) -> Simple Traversal (k, v) v
value p f kv@(k,v)
  | p k       = (,) k <$> f v
  | otherwise = pure kv
{-# INLINE value #-}
