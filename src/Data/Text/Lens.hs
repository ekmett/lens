{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Text.Lens
  ( IsText(..)
  ) where

import           Control.Lens
import           Data.Text as Strict
import qualified Data.Text.Strict.Lens as Strict
import           Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Lens as Lazy
import           Data.Text.Lazy.Builder

-- | Traversals for strict or lazy 'Text'
class IsText t where
  -- | 'pack' (or 'unpack') strict or lazy 'Text'.
  --
  -- @
  -- 'pack' x = x '^.' 'packed'
  -- 'unpack' x = x '^.' 'from' 'packed'
  -- @
  packed :: Iso' String t

  -- | Convert between strict or lazy 'Text' and a 'Builder'.
  --
  -- @
  -- 'fromText' x = x '^.' 'builder'
  -- @
  builder :: Iso' t Builder

  -- | Traverse the individual characters in strict or lazy 'Text'.
  text :: IndexedTraversal' Int t Char
  text = from packed . itraversed
  {-# INLINE text #-}

instance IsText Strict.Text where
  packed = Strict.packed
  {-# INLINE packed #-}
  builder = Strict.builder
  {-# INLINE builder #-}
  text = Strict.text
  {-# INLINE text #-}

instance IsText Lazy.Text where
  packed = Lazy.packed
  {-# INLINE packed #-}
  builder = Lazy.builder
  {-# INLINE builder #-}
  text = Lazy.text
  {-# INLINE text #-}

