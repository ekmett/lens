{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.ByteString.Lens
  ( IsByteString(..)
  ) where

import           Control.Lens
import           Data.Word (Word8)
import           Data.ByteString as Strict
import qualified Data.ByteString.Strict.Lens as Strict
import           Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Lens as Lazy

-- | Traversals for ByteStrings.
class IsByteString t where
  -- | 'Data.ByteString.pack' (or 'Data.ByteString.unpack') a list of bytes into a strict or lazy 'ByteString'.
  --
  -- @'Data.ByteString.pack' x = x '^.' 'packedBytes'@
  --
  -- @'Data.ByteString.unpack' x = x '^.' 'from' 'packedBytes'@
  packedBytes :: Iso' [Word8] t

  -- | 'Data.ByteString.Char8.pack' (or 'Data.ByteString.Char8.unpack') a list of characters into a strict or lazy 'ByteString'.
  --
  -- When writing back to the 'ByteString' it is assumed that every 'Char'
  -- lies between '\x00' and '\xff'.
  --
  -- @'Data.ByteString.Char8.pack' x = x '^.' 'packedChars'@
  --
  -- @'Data.ByteString.Char8.unpack' x = x '^.' 'from' 'packedChars'@
  packedChars :: Iso' String t

  -- | Traverse each 'Word8' in a strict or lazy 'ByteString'
  --
  -- @'bytes' = 'from' 'packedBytes' '.' 'itraversed'@
  --
  -- @'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'@
  bytes :: IndexedTraversal' Int t Word8
  bytes = from packedBytes . traversed
  {-# INLINE bytes #-}

  -- | Traverse the individual bytes in a strict or lazy 'ByteString' as characters.
  --
  -- When writing back to the 'ByteString' it is assumed that every 'Char'
  -- lies between '\x00' and '\xff'.
  --
  -- @'chars' = 'from' 'packedChars' '.' 'traverse'@
  --
  -- @'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'@
  chars :: IndexedTraversal' Int t Char
  chars = from packedChars . traversed
  {-# INLINE chars #-}

instance IsByteString Strict.ByteString where
  packedBytes = Strict.packedBytes
  {-# INLINE packedBytes #-}
  packedChars = Strict.packedChars
  {-# INLINE packedChars #-}
  bytes = Strict.bytes
  {-# INLINE bytes #-}
  chars = Strict.chars
  {-# INLINE chars #-}

instance IsByteString Lazy.ByteString where
  packedBytes = Lazy.packedBytes
  {-# INLINE packedBytes #-}
  packedChars = Lazy.packedChars
  {-# INLINE packedChars #-}
  bytes = from packedBytes . traversed
  {-# INLINE bytes #-}
  chars = from packedChars . traversed
  {-# INLINE chars #-}
