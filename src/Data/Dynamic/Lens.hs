-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Dynamic.Lens
  ( traverseDynamic
  ) where

import Control.Applicative
import Control.Lens
import Data.Dynamic

-- |
-- Traverse the typed value contained in a 'Dynamic' where the type required by your function matches that
-- of the contents of the 'Dynamic'.
--
-- @'traverseDynamic' :: ('Applicative' f, 'Typeable' a, 'Typeable' b) => (a -> f b) -> 'Dynamic' -> f 'Dynamic'@
traverseDynamic :: (Typeable a, Typeable b) => Traversal Dynamic Dynamic a b
traverseDynamic f dyn = case fromDynamic dyn of
  Just a  -> toDyn <$> f a
  Nothing -> pure dyn
{-# INLINE traverseDynamic #-}
