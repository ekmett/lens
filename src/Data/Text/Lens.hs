-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Text.Lens
  ( TraverseText(..)
  ) where

import Control.Lens
import Data.Text as Strict
import Data.Text.Lazy as Lazy

-- | Provides ad hoc overloading for 'traverseText' for both strict and lazy 'Text'.
class TraverseText t where
  -- | Traverse the individual characters in a either strict or lazy 'Text'.
  --
  -- > anyOf traverseText (=='c') :: TraverseText b => b -> Bool
  traverseText :: Simple Traversal t Char

instance TraverseText Strict.Text where
  traverseText f = fmap Strict.pack . traverse f . Strict.unpack

instance TraverseText Lazy.Text where
  traverseText f = fmap Lazy.pack . traverse f . Lazy.unpack
