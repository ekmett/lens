{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lazy.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Text.Lazy.Lens
  ( packed
  , text
  , builder
  ) where

import Control.Lens
import Data.Text.Lazy
import Data.Text.Lazy.Builder

-- | Pack (or unpack) lazy 'Text'.
--
-- @
-- 'pack' x = x '^.' 'packed'
-- 'unpack' x = x '^.' 'from' 'packed'
-- @
packed :: Iso' String Text
packed = iso pack unpack
{-# INLINE packed #-}

-- | Convert between lazy 'Text' and 'Builder' .
--
-- @
-- 'fromLazyText' x = x '^.' 'builder'
-- 'toLazyText' x = x '^.' 'from' 'builder'
-- @
builder :: Iso' Text Builder
builder = iso fromLazyText toLazyText
{-# INLINE builder #-}

-- | Traverse the individual characters in a 'Text'.
--
-- > anyOf text (=='c') :: Text -> Bool
text :: IndexedTraversal' Int Text Char
text = from packed . itraversed
{-# INLINE text #-}
