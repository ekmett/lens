{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lazy.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Lenses for lazy bytestrings
----------------------------------------------------------------------------
module Data.ByteString.Lazy.Lens
  ( packedBytes, bytes
  , packedChars, chars
  ) where

import Control.Lens
import Data.ByteString.Lazy as Words
import Data.ByteString.Lazy.Char8 as Char8
import Data.Word (Word8)

-- | 'Data.ByteString.Lazy.pack' (or 'Data.ByteString.Lazy.unpack') a list of bytes into a 'ByteString'
--
-- @'Data.ByteString.Lazy.pack' x = x '^.' 'packedBytes'@
--
-- @'Data.ByteString.Lazy.unpack' x = x '^.' 'from' 'packedBytes'@
packedBytes :: Simple Iso [Word8] ByteString
packedBytes = iso Words.pack Words.unpack
{-# INLINE packedBytes #-}

-- | Traverse the individual bytes in a 'ByteString'
--
-- @'bytes' = 'from' 'packedBytes' . 'itraversed'@
--
-- @'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'@
bytes :: SimpleIndexedTraversal Int ByteString Word8
bytes = from packedBytes .> itraversed
{-# INLINE bytes #-}

-- | 'Data.ByteString.Lazy.Char8.pack' (or 'Data.ByteString.Lazy.Char8.unpack') a list of characters into a 'ByteString'
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'Data.ByteString.Lazy.Char8.pack' x = x '^.' 'packedChars'@
--
-- @'Data.ByteString.Lazy.Char8.unpack' x = x '^.' 'from' 'packedChars'@
packedChars :: Simple Iso String ByteString
packedChars = iso Char8.pack Char8.unpack
{-# INLINE packedChars #-}

-- | Traverse the individual bytes in a 'ByteString' as characters.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'chars' = 'from' 'packedChars' '.>' 'itraversed'@
--
-- @'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'@
chars :: SimpleIndexedTraversal Int ByteString Char
chars = from packedChars .> itraversed
{-# INLINE chars #-}
