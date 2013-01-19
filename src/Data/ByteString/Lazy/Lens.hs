{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lazy.Lens
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Lenses for lazy bytestrings
----------------------------------------------------------------------------
module Data.ByteString.Lazy.Lens
  ( packedBytes, bytes
  , packedChars, chars
  ) where

import Control.Lens
import Control.Lens.Internal.ByteString
import Data.ByteString.Lazy       as Words
import Data.ByteString.Lazy.Char8 as Char8
import Data.Word (Word8)
import Data.Int (Int64)

-- | 'Data.ByteString.Lazy.pack' (or 'Data.ByteString.Lazy.unpack') a list of bytes into a 'ByteString'.
--
-- @'Data.ByteString.Lazy.pack' x = x '^.' 'packedBytes'@
--
-- @'Data.ByteString.Lazy.unpack' x = x '^.' 'from' 'packedBytes'@
packedBytes :: Iso' [Word8] ByteString
packedBytes = iso Words.pack unpackLazy
{-# INLINE packedBytes #-}

-- | Traverse the individual bytes in a 'ByteString'.
--
-- @'bytes' = 'from' 'packedBytes' '.' 'itraversed'@
--
-- @'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'@
bytes :: IndexedTraversal' Int64 ByteString Word8
bytes = traversedLazy
{-# INLINE bytes #-}

-- | 'Data.ByteString.Lazy.Char8.pack' (or 'Data.ByteString.Lazy.Char8.unpack') a list of characters into a 'ByteString'.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'Data.ByteString.Lazy.Char8.pack' x = x '^.' 'packedChars'@
--
-- @'Data.ByteString.Lazy.Char8.unpack' x = x '^.' 'from' 'packedChars'@
packedChars :: Iso' String ByteString
packedChars = iso Char8.pack unpackLazy8
{-# INLINE packedChars #-}

-- | Traverse the individual bytes in a 'ByteString' as characters.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'chars' = 'from' 'packedChars' '.' 'itraversed'@
--
-- @'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'@
chars :: IndexedTraversal' Int64 ByteString Char
chars = traversedLazy8
{-# INLINE chars #-}
