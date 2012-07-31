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

-- | Pack (or unpack) a list of bytes into a 'ByteString'
--
-- > pack x = x^.packedBytes
-- > unpack x = x^.from packedBytes
packedBytes :: Simple Iso [Word8] ByteString
packedBytes = iso Words.pack Words.unpack
{-# INLINE packedBytes #-}
{-# SPECIALIZE packedBytes :: Simple Lens [Word8] ByteString #-}

-- | Traverse the individual bytes in a 'ByteString'
--
-- > bytes = from packedBytes . traverse
--
-- > anyOf bytes (==0x80) :: ByteString -> Bool
bytes :: Simple Traversal ByteString Word8
bytes = from packedBytes . traverse
{-# INLINE bytes #-}

-- | Pack (or unpack) a list of characters into a 'ByteString'
--
-- When writing back to the byteString it is assumed that all characters
-- lie between '\x00' and '\xff'.
--
-- > pack x = x^.packedChars
-- > unpack x = x^.from packedChars
packedChars :: Simple Iso String ByteString
packedChars = iso Char8.pack Char8.unpack
{-# INLINE packedChars #-}
{-# SPECIALIZE packedChars :: Simple Lens String ByteString #-}

-- | Traverse the individual bytes in a 'ByteString' as characters.
--
-- When writing back to the byteString it is assumed that all characters
-- lie between '\x00' and '\xff'.
--
-- > chars = from packed . traverse
--
-- > anyOf chars (=='c') :: ByteString -> Bool
chars :: Simple Traversal ByteString Char
chars = from packedChars . traverse
{-# INLINE chars #-}
