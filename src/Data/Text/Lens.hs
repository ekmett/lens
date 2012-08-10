{-# LANGUAGE FlexibleContexts #-}
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
  ( packed
  , text
  ) where

import Control.Lens
import Data.Text
import Data.List.Lens

-- | Pack (or unpack) 'Text'.
--
-- > pack x = x^.packed
-- > unpack x = x^.from packed
packed :: Simple Iso String Text
packed = iso pack unpack
{-# INLINE packed #-}
{-# SPECIALIZE packed :: Simple Lens String Text #-}

-- | Traverse the individual characters in a either strict or lazy 'Text'.
--
-- > anyOf text (=='c') :: Text -> Bool
text :: SimpleIndexedTraversal Int Text Char
text = from packed .> traverseList
{-# INLINE text #-}
