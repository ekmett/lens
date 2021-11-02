{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.ByteString.Lens
  ( IsByteString(..)
  , unpackedBytes
  , unpackedChars
  , pattern Bytes
  , pattern Chars
  ) where

import           Control.Lens
import           Data.Word (Word8)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Strict.Lens as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Lens as Lazy

-- | Traversals for ByteStrings.
class IsByteString t where
  -- | 'Data.ByteString.pack' (or 'Data.ByteString.unpack') a list of bytes into a strict or lazy 'ByteString'.
  --
  -- @
  -- 'Data.ByteString.pack' x ≡ x '^.' 'packedBytes'
  -- 'Data.ByteString.unpack' x ≡ x '^.' 'from' 'packedBytes'
  -- 'packedBytes' ≡ 'from' 'unpackedBytes'
  -- @
  packedBytes :: Iso' [Word8] t

  -- | 'Data.ByteString.Char8.pack' (or 'Data.ByteString.Char8.unpack') a list of characters into a strict or lazy 'ByteString'.
  --
  -- When writing back to the 'ByteString' it is assumed that every 'Char'
  -- lies between @'\x00'@ and @'\xff'@.
  --
  -- @
  -- 'Data.ByteString.Char8.pack' x ≡ x '^.' 'packedChars'
  -- 'Data.ByteString.Char8.unpack' x ≡ x '^.' 'from' 'packedChars'
  -- 'packedChars' ≡ 'from' 'unpackedChars'
  -- @
  packedChars :: Iso' String t

  -- | Traverse each 'Word8' in a strict or lazy 'ByteString'
  --
  --
  -- This 'Traversal' walks each strict 'ByteString' chunk in a tree-like fashion
  -- enable zippers to seek to locations more quickly and accelerate
  -- many monoidal queries, but up to associativity (and constant factors) it is
  -- equivalent to the much slower:
  --
  -- @
  -- 'bytes' ≡ 'unpackedBytes' '.' 'traversed'
  -- @
  --
  -- @
  -- 'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'
  -- @
  bytes :: IndexedTraversal' Int t Word8
  bytes = from packedBytes . traversed
  {-# INLINE bytes #-}

  -- | Traverse the individual bytes in a strict or lazy 'ByteString' as characters.
  --
  -- When writing back to the 'ByteString' it is assumed that every 'Char'
  -- lies between @'\x00'@ and @'\xff'@.
  --
  -- This 'Traversal' walks each strict 'ByteString' chunk in a tree-like fashion
  -- enable zippers to seek to locations more quickly and accelerate
  -- many monoidal queries, but up to associativity (and constant factors) it is
  -- equivalent to the much slower:
  --
  -- @
  -- 'chars' ≡ 'unpackedChars' '.' 'traversed'
  -- @
  --
  -- @
  -- 'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'
  -- @
  chars :: IndexedTraversal' Int t Char
  chars = from packedChars . traversed
  {-# INLINE chars #-}

-- | 'Data.ByteString.unpack' (or 'Data.ByteString.pack') a 'ByteString' into a list of bytes
--
-- @
-- 'unpackedBytes' ≡ 'from' 'packedBytes'
-- 'Data.ByteString.unpack' x ≡ x '^.' 'unpackedBytes'
-- 'Data.ByteString.pack' x ≡  x '^.' 'from' 'unpackedBytes'
-- @
--
-- @
-- 'unpackedBytes' :: 'Iso'' 'Data.ByteString.ByteString' ['Word8']
-- 'unpackedBytes' :: 'Iso'' 'Data.ByteString.Lazy.ByteString' ['Word8']
-- @
unpackedBytes :: IsByteString t => Iso' t [Word8]
unpackedBytes = from packedBytes
{-# INLINE unpackedBytes #-}

pattern Bytes :: IsByteString s => [Word8] -> s
pattern Bytes b <- (view unpackedBytes -> b) where
  Bytes b = review unpackedBytes b

pattern Chars :: IsByteString s => String -> s
pattern Chars b <- (view unpackedChars -> b) where
  Chars b = review unpackedChars b

-- | 'Data.ByteString.Char8.unpack' (or 'Data.ByteString.Char8.pack') a list of characters into a strict (or lazy) 'ByteString'
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between @'\x00'@ and @'\xff'@.
--
-- @
-- 'unpackedChars' ≡ 'from' 'packedChars'
-- 'Data.ByteString.Char8.unpack' x ≡ x '^.' 'unpackedChars'
-- 'Data.ByteString.Char8.pack' x ≡ x '^.' 'from' 'unpackedChars'
-- @
--
-- @
-- 'unpackedChars' :: 'Iso'' 'Data.ByteString.ByteString' 'String'
-- 'unpackedChars' :: 'Iso'' 'Data.ByteString.Lazy.ByteString' 'String'
-- @
unpackedChars :: IsByteString t => Iso' t String
unpackedChars = from packedChars
{-# INLINE unpackedChars #-}

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
