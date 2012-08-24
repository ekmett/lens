{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Typeable.Lens
  ( _cast
  ) where

import Control.Applicative
import Control.Lens
import Data.Typeable
import Unsafe.Coerce as Unsafe

_cast :: (Typeable a, Typeable b) => Simple Traversal a b
_cast f a = case cast a of
  Just b -> Unsafe.unsafeCoerce <$> f b
  Nothing -> pure a

