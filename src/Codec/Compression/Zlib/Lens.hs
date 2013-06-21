{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Compression.Zlib.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- @lens@ support for the @zlib@ library
----------------------------------------------------------------------------
module Codec.Compression.Zlib.Lens
  ( -- * High-Level API
    gzipped,
    zlibbed,
    deflated,
    compressed,
    Format,
    gzip,
    zlib,
    deflate,
    -- * Low-Level API
    zlibbed',
    gzipped',
    deflated',
    compressed',
    Params,
    defaultParams,
    levelC,
    methodC,
    windowBitsC,
    windowBitsD,
    memoryLevelC,
    strategyC,
    bufferSizeC,
    bufferSizeD,
    dictionary,
    CompressionLevel,
    defaultCompression,
    noCompression,
    bestSpeed,
    bestCompression,
    compressionLevel,
    Method,
    deflateMethod,
    WindowBits,
    defaultWindowBits,
    windowBits,
    MemoryLevel,
    defaultMemoryLevel,
    minMemoryLevel,
    maxMemoryLevel,
    memoryLevel,
    CompressionStrategy,
    defaultStrategy,
    filteredStrategy,
    huffmanOnlyStrategy
  ) where

import Control.Applicative
import Codec.Compression.Zlib.Internal
import Control.Lens
import qualified Data.ByteString      as S (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)

-- |
-- The 'zlib' compression format.
zlib :: Format
zlib = zlibFormat

-- |
-- The 'gzip' compression format.
gzip :: Format
gzip = gzipFormat

-- |
-- The 'deflate' compression format.
deflate :: Format
deflate = rawFormat

-- |
-- Compresses a 'L.ByteString' using the 'gzip' compression format.
--
-- @'gzipped' = 'compressed' 'gzip'@
-- @'gzipped' = 'gzipped\'' 'defaultParams'
gzipped :: Iso' L.ByteString L.ByteString
gzipped = compressed gzip
{-# INLINE gzipped #-}

-- |
-- Compresses a 'L.ByteString' using the 'zlib' compression format.
--
-- @'zlibbed' = 'compressed' 'zlib'@
-- @'zlibbed' = 'zlibbed\'' 'defaultParams'
zlibbed :: Iso' L.ByteString L.ByteString
zlibbed = compressed zlib
{-# INLINE zlibbed #-}

-- |
-- Compresses a 'L.ByteString' using the 'deflate' compression format.
--
-- @'deflated' = 'compressed' 'deflate'@
-- @'deflated' = 'deflated\'' 'defaultParams'
deflated :: Iso' L.ByteString L.ByteString
deflated = compressed deflate
{-# INLINE deflated #-}

-- |
-- Compresses a 'L.ByteString' using the given compression format.
--
-- @'compressed' fmt = 'compressed\'' fmt 'defaultParams'@
compressed :: Format -> Iso' L.ByteString L.ByteString
compressed fmt = compressed' fmt defaultParams
{-# INLINE compressed #-}

-- |
-- Compresses a 'L.ByteString' using the 'gzip' compression format and the given advanced parameters.
--
-- @'gzipped' = 'compressed' 'gzip'@
-- @'gzipped' = 'gzipped\'' 'defaultParams'
gzipped' :: Params -> Iso' L.ByteString L.ByteString
gzipped' = compressed' gzip
{-# INLINE gzipped' #-}

-- |
-- Compresses a 'L.ByteString' using the 'zlib' compression format and the given advanced parameters.
--
-- @'zlibbed' = 'compressed' 'zlib'@
-- @'zlibbed' = 'zlibbed\'' 'defaultParams'
zlibbed' :: Params -> Iso' L.ByteString L.ByteString
zlibbed' = compressed' zlib
{-# INLINE zlibbed' #-}

-- |
-- Compresses a 'L.ByteString' using the 'deflate' compression format and the given advanced parameters.
--
-- @'deflated' = 'compressed' 'deflate'@
-- @'deflated' = 'deflated\'' 'defaultParams'
deflated' :: Params -> Iso' L.ByteString L.ByteString
deflated' = compressed' deflate
{-# INLINE deflated' #-}

-- |
-- Compresses a 'L.ByteString' using the given compression format and the given advanced parameters.
compressed' :: Format -> Params -> Iso' L.ByteString L.ByteString
compressed' fmt (Params c d) = iso (compress fmt c) (decompress fmt d)
{-# INLINE compressed' #-}

-- |
-- The advanced parameters needed by 'gzipped\'', 'zlibbed\'', 'deflated\'', and 'compressed\''.
--
-- Use 'defaultParams' and the provided 'Lens'es to construct custom 'Params'.
data Params = Params !CompressParams !DecompressParams

-- |
-- The default advanced parameters for compression and decompression.
defaultParams :: Params
defaultParams = Params defaultCompressParams defaultDecompressParams
{-# INLINE defaultParams #-}

-- |
-- The compression level.
levelC :: Lens' Params CompressionLevel
levelC = \ f (Params c d) -> (\l -> Params (c {compressLevel = l}) d) <$> f (compressLevel c)
{-# INLINE levelC #-}

-- |
-- The compression method.
methodC :: Lens' Params Method
methodC = \ f (Params c d) -> (\m -> Params (c {compressMethod = m}) d) <$> f (compressMethod c)
{-# INLINE methodC #-}

-- |
-- The number of bits in the compression window.
windowBitsC :: Lens' Params WindowBits
windowBitsC = \ f (Params c d) -> (\wb -> Params (c {compressWindowBits = wb}) d) <$> f (compressWindowBits c)
{-# INLINE windowBitsC #-}

-- |
-- The number of bits in the decompression window.
windowBitsD :: Lens' Params WindowBits
windowBitsD = \ f (Params c d) -> (\wb -> Params c (d {decompressWindowBits = wb})) <$> f (decompressWindowBits d)
{-# INLINE windowBitsD #-}

-- |
-- The amount of memory allowed for the internal compression state.
memoryLevelC :: Lens' Params MemoryLevel
memoryLevelC = \ f (Params c d) -> (\ml -> Params (c {compressMemoryLevel = ml}) d) <$> f (compressMemoryLevel c)
{-# INLINE memoryLevelC #-}

-- |
-- The compression strategy.
strategyC :: Lens' Params CompressionStrategy
strategyC = \ f (Params c d) -> (\s -> Params (c {compressStrategy = s}) d) <$> f (compressStrategy c)
{-# INLINE strategyC #-}

-- |
-- The initial buffer size during compression.
bufferSizeC :: Lens' Params Int
bufferSizeC = \ f (Params c d) -> (\bs -> Params (c {compressBufferSize = bs}) d) <$> f (compressBufferSize c)
{-# INLINE bufferSizeC #-}

-- |
-- The initial buffer size during decompression.
bufferSizeD :: Lens' Params Int
bufferSizeD = \ f (Params c d) -> (\bs -> Params c (d {decompressBufferSize = bs})) <$> f (decompressBufferSize d)
{-# INLINE bufferSizeD #-}

-- |
-- 'Just' the custom (de)compression dictionary to use, or 'Nothing' to not use a custom dictionary.
dictionary :: Lens' Params (Maybe S.ByteString)
dictionary = \f (Params c d) -> (\mbs -> Params (c {compressDictionary = mbs}) (d {decompressDictionary = mbs})) <$> f (compressDictionary c <|> decompressDictionary d)
{-# INLINE dictionary #-}
