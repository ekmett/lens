{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lazy.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Text.Lazy.Lens
  ( packed
  , text
  ) where

import Control.Lens
import Data.Text.Lazy

-- | Pack (or unpack) lazy 'Text'.
--
-- @
-- 'pack' x = x '^.' 'packed'
-- 'unpack' x = x '^.' 'from' 'packed'
-- @
packed :: Simple Iso String Text
packed = iso pack unpack
{-# INLINE packed #-}

-- | Traverse the individual characters in a 'Text'.
--
-- > anyOf text (=='c') :: Text -> Bool
text :: SimpleIndexedTraversal Int Text Char
text = from packed .> itraversed
{-# INLINE text #-}
