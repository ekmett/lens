{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Strict.Lens
-- Copyright   :  (C) 2012-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Text.Strict.Lens
  ( packed, unpacked
  , builder
  , text
  , utf8
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder

-- $setup
-- >>> :set -XOverloadedStrings

-- | This isomorphism can be used to 'pack' (or 'unpack') strict 'Text'.
--
--
-- >>> "hello"^.packed -- :: Text
-- "hello"
--
-- @
-- 'pack' x ≡ x '^.' 'packed'
-- 'unpack' x ≡ x '^.' 'from' 'packed'
-- 'packed' ≡ 'from' 'unpacked'
-- 'packed' ≡ 'iso' 'pack' 'unpack'
-- @
packed :: Iso' String Text
packed = iso pack unpack
{-# INLINE packed #-}

-- | This isomorphism can be used to 'unpack' (or 'pack') lazy 'Text'.
--
-- >>> "hello"^.unpacked -- :: String
-- "hello"
--
-- This 'Iso' is provided for notational convenience rather than out of great need, since
--
-- @
-- 'unpacked' ≡ 'from' 'packed'
-- @
--
-- @
-- 'pack' x ≡ x '^.' 'from' 'unpacked'
-- 'unpack' x ≡ x '^.' 'packed'
-- 'unpacked' ≡ 'iso' 'unpack' 'pack'
-- @
unpacked :: Iso' Text String
unpacked = iso unpack pack

-- | Convert between strict 'Text' and 'Builder' .
--
-- @
-- 'fromText' x ≡ x '^.' 'builder'
-- 'toStrict' ('toLazyText' x) ≡ x '^.' 'from' 'builder'
-- @
builder :: Iso' Text Builder
builder = iso fromText (toStrict . toLazyText)
{-# INLINE builder #-}

-- | Traverse the individual characters in strict 'Text'.
--
-- >>> anyOf text (=='o') "hello"
-- True
--
-- When the type is unambiguous, you can also use the more general 'each'.
--
-- @
-- 'text' ≡ 'unpacked' . 'traversed'
-- 'text' ≡ 'each'
-- @
--
-- Note that when just using this as a 'Setter', @'setting' 'Data.Text.map'@ can
-- be more efficient.
text :: IndexedTraversal' Int Text Char
text = unpacked . traversed
{-# INLINE text #-}

-- | Encode/Decode a strict 'Text' to/from strict 'ByteString', via UTF-8.
--
-- >>> utf8 # "☃"
-- "\226\152\131"
utf8 :: Prism' ByteString Text
utf8 = prism' encodeUtf8 (preview _Right . decodeUtf8')
{-# INLINE utf8 #-}
