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
  ( IsText(..)
  , traverseText
  ) where

import Control.Lens
import Data.Text as Strict
import Data.Text.Lazy as Lazy

-- | Provides ad hoc overloading for 'traverseText' for both strict and lazy 'Text'.
class IsText t where
  -- | Pack (or unpack) 'Text'.
  --
  -- > pack x = x^.packedText
  -- > unpack x = x^.from packedText
  packedText :: Simple Iso String t

instance IsText Strict.Text where
  packedText = iso Strict.pack Strict.unpack
  {-# INLINE packedText #-}

instance IsText Lazy.Text where
  packedText = iso Lazy.pack Lazy.unpack
  {-# INLINE packedText #-}

-- | Traverse the individual characters in a either strict or lazy 'Text'.
--
-- > anyOf traverseText (=='c') :: TraverseText b => b -> Bool
traverseText :: IsText t => Simple Traversal t Char
traverseText = from packedText . traverse
{-# INLINE traverseText #-}
