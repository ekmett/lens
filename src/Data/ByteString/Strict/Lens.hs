{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Strict.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.ByteString.Strict.Lens
  ( packedBytes, bytes
  , packedChars, chars
  ) where

import Control.Lens
import Data.ByteString as Words
import Data.ByteString.Char8 as Char8
import Data.Word (Word8)

-- | 'Data.ByteString.pack' (or 'Data.ByteString.unpack') a list of bytes into a 'ByteString'
--
-- @'Data.ByteString.pack' x = x '^.' 'packedBytes'@
--
-- @'Data.ByteString.unpack' x = x '^.' 'from' 'packedBytes'@
packedBytes :: Simple Iso [Word8] ByteString
packedBytes = iso Words.pack Words.unpack
{-# INLINE packedBytes #-}

-- | Traverse each 'Word8' in a 'ByteString'
--
-- @'bytes' = 'from' 'packedBytes' '.>' 'itraversed'@
--
-- @'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'@
bytes :: SimpleIndexedTraversal Int ByteString Word8
bytes = from packedBytes .> itraversed
{-# INLINE bytes #-}

-- | 'Data.ByteString.Char8.pack' (or 'Data.ByteString.Char8.unpack') a list of characters into a 'ByteString'
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'Data.ByteString.Char8.pack' x = x '^.' 'packedChars'@
--
-- @'Data.ByteString.Char8.unpack' x = x '^.' 'from' 'packedChars'@
packedChars :: Simple Iso String ByteString
packedChars = iso Char8.pack Char8.unpack
{-# INLINE packedChars #-}

-- | Traverse the individual bytes in a 'ByteString' as characters.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'chars' = 'from' 'packedChars' . 'traverse'@
--
-- @'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'@
chars :: SimpleIndexedTraversal Int ByteString Char
chars = from packedChars .> itraversed
{-# INLINE chars #-}
