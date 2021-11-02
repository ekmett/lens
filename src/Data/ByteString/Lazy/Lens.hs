{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lazy.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Lazy 'ByteString' lenses.
----------------------------------------------------------------------------
module Data.ByteString.Lazy.Lens
  ( packedBytes, unpackedBytes, bytes
  , packedChars, unpackedChars, chars
  , pattern Bytes
  , pattern Chars
  ) where

import Control.Lens
import Control.Lens.Internal.ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy       as Words
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Word (Word8)
import Data.Int (Int64)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> import Numeric.Lens
-- >>> import qualified Data.ByteString.Lazy.Char8 as Char8

-- | 'Data.ByteString.Lazy.pack' (or 'Data.ByteString.Lazy.unpack') a list of bytes into a 'ByteString'.
--
-- @
-- 'packedBytes' ≡ 'from' 'unpackedBytes'
-- 'Data.ByteString.pack' x ≡  x '^.' 'packedBytes'
-- 'Data.ByteString.unpack' x ≡ x '^.' 'from' 'packedBytes'
-- @
--
-- >>> [104,101,108,108,111]^.packedBytes == Char8.pack "hello"
-- True
packedBytes :: Iso' [Word8] ByteString
packedBytes = iso Words.pack Words.unpack
{-# INLINE packedBytes #-}

-- | 'Data.ByteString.Lazy.unpack' (or 'Data.ByteString.Lazy.pack') a 'ByteString' into a list of bytes
--
-- @
-- 'unpackedBytes' ≡ 'from' 'packedBytes'
-- 'Data.ByteString.unpack' x ≡ x '^.' 'unpackedBytes'
-- 'Data.ByteString.pack' x ≡  x '^.' 'from' 'unpackedBytes'
-- @
--
-- >>> "hello"^.packedChars.unpackedBytes
-- [104,101,108,108,111]
unpackedBytes :: Iso' ByteString [Word8]
unpackedBytes = from packedBytes
{-# INLINE unpackedBytes #-}

-- | Traverse the individual bytes in a 'ByteString'.
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
-- >>> anyOf bytes (== 0x80) (Char8.pack "hello")
-- False
--
-- Note that when just using this as a 'Setter', @'setting' 'Data.ByteString.Lazy.map'@
-- can be more efficient.
bytes :: IndexedTraversal' Int64 ByteString Word8
bytes = traversedLazy
{-# INLINE bytes #-}

-- | 'Data.ByteString.Lazy.Char8.pack' (or 'Data.ByteString.Lazy.Char8.unpack') a list of characters into a 'ByteString'.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between @'\x00'@ and @'\xff'@.
--
-- @
-- 'packedChars' ≡ 'from' 'unpackedChars'
-- 'Data.ByteString.Char8.pack' x ≡ x '^.' 'packedChars'
-- 'Data.ByteString.Char8.unpack' x ≡ x '^.' 'from' 'packedChars'
-- @
--
-- >>> "hello"^.packedChars.each.re (base 16 . enum).to (\x -> if Prelude.length x == 1 then '0':x else x)
-- "68656c6c6f"
packedChars :: Iso' String ByteString
packedChars = iso Char8.pack Char8.unpack
{-# INLINE packedChars #-}

-- | 'Data.ByteString.Lazy.Char8.unpack' (or 'Data.ByteString.Lazy.Char8.pack') a list of characters into a 'ByteString'
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
-- >>> [104,101,108,108,111]^.packedBytes.unpackedChars
-- "hello"
unpackedChars :: Iso' ByteString String
unpackedChars = from packedChars
{-# INLINE unpackedChars #-}

-- | Traverse the individual bytes in a 'ByteString' as characters.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between @'\x00'@ and @'\xff'@.
--
-- This 'Traversal' walks each strict 'ByteString' chunk in a tree-like fashion
-- enable zippers to seek to locations more quickly and accelerate
-- many monoidal queries, but up to associativity (and constant factors) it is
-- equivalent to:
--
-- @
-- 'chars' = 'unpackedChars' '.' 'traversed'
-- @
--
-- >>> anyOf chars (== 'h') "hello"
-- True
chars :: IndexedTraversal' Int64 ByteString Char
chars = traversedLazy8
{-# INLINE chars #-}

pattern Bytes :: [Word8] -> ByteString
pattern Bytes b <- (view unpackedBytes -> b) where
  Bytes b = review unpackedBytes b

pattern Chars :: String -> ByteString
pattern Chars b <- (view unpackedChars -> b) where
  Chars b = review unpackedChars b
