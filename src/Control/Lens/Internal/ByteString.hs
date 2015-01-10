{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#ifndef MIN_VERSION_bytestring
#define MIN_VERSION_bytestring(x,y,z) 1
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-} -- for inlinePerformIO

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Strict.Lens
-- Copyright   :  (C) 2012-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module spends a lot of time fiddling around with 'Data.ByteString' internals
-- to work around <http://hackage.haskell.org/trac/ghc/ticket/7556> on older
-- Haskell Platforms and to improve constant and asymptotic factors in our performance.
----------------------------------------------------------------------------
module Control.Lens.Internal.ByteString
  ( unpackStrict, traversedStrictTree
  , unpackStrict8, traversedStrictTree8
  , unpackLazy, traversedLazy
  , unpackLazy8, traversedLazy8
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Unsafe        as BU
import Data.Bits
import Data.Char
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Storable
#if MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
#elif MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
#else
import Foreign.ForeignPtr
#endif
import GHC.Base (unsafeChr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)

grain :: Int
grain = 32
{-# INLINE grain #-}

-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a balanced tree with biased runs of
-- elements at the leaves.
traversedStrictTree :: IndexedTraversal' Int B.ByteString Word8
traversedStrictTree pafb bs = unsafeCreate len <$> go 0 len
 where
   len = B.length bs
   go !i !j
     | i + grain < j, k <- i + shiftR (j - i) 1 = (\l r q -> l q >> r q) <$> go i k <*> go k j
     | otherwise = run i j
   run !i !j
     | i == j    = pure (\_ -> return ())
     | otherwise = let !x = BU.unsafeIndex bs i
                   in (\y ys !q -> pokeByteOff q i y >> ys q) <$> indexed pafb (i :: Int) x <*> run (i + 1) j
{-# INLINE traversedStrictTree #-}


-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a balanced tree with biased runs of
-- elements at the leaves, pretending the bytes are chars.
traversedStrictTree8 :: IndexedTraversal' Int B.ByteString Char
traversedStrictTree8 pafb bs = unsafeCreate len <$> go 0 len
 where
   len = B.length bs
   go !i !j
     | i + grain < j, k <- i + shiftR (j - i) 1 = (\l r q -> l q >> r q) <$> go i k <*> go k j
     | otherwise = run i j
   run !i !j
     | i == j    = pure (\_ -> return ())
     | otherwise = let !x = BU.unsafeIndex bs i
                   in (\y ys q -> poke (q `plusPtr` i) (c2w y) >> ys q) <$> indexed pafb (i :: Int) (w2c x) <*> run (i + 1) j
{-# INLINE traversedStrictTree8 #-}

-- | Unpack a lazy 'Bytestring'
unpackLazy :: BL.ByteString -> [Word8]
unpackLazy = BL.unpack
{-# INLINE unpackLazy #-}

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString'
traversedLazy :: IndexedTraversal' Int64 BL.ByteString Word8
traversedLazy pafb = \lbs -> foldrChunks go (\_ -> pure BL.empty) lbs 0
  where
  go c fcs acc = BL.append . fromStrict
             <$> reindexed (\x -> acc + fromIntegral x :: Int64) traversedStrictTree pafb c
             <*> fcs acc'
    where
    acc' :: Int64
    !acc' = acc + fromIntegral (B.length c)
{-# INLINE traversedLazy #-}


-- | Unpack a lazy 'BL.ByteString' pretending the bytes are chars.
unpackLazy8 :: BL.ByteString -> String
unpackLazy8 = BL8.unpack
{-# INLINE unpackLazy8 #-}

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString' pretending the bytes are chars.
traversedLazy8 :: IndexedTraversal' Int64 BL.ByteString Char
traversedLazy8 pafb = \lbs -> foldrChunks go (\_ -> pure BL.empty) lbs 0
  where
  go c fcs acc = BL.append . fromStrict
             <$> reindexed (\x -> acc + fromIntegral x :: Int64) traversedStrictTree8 pafb c
             <*> fcs acc'
    where
    acc' :: Int64
    !acc' = acc + fromIntegral (B.length c)
{-# INLINE traversedLazy8 #-}

------------------------------------------------------------------------------
-- ByteString guts
------------------------------------------------------------------------------

fromStrict :: B.ByteString -> BL.ByteString
#if MIN_VERSION_bytestring(0,10,0)
fromStrict = BL.fromStrict
#else
fromStrict = \x -> BL.fromChunks [x]
#endif
{-# INLINE fromStrict #-}

foldrChunks :: (B.ByteString -> r -> r) -> r -> BL.ByteString -> r
#if MIN_VERSION_bytestring(0,10,0)
foldrChunks = BL.foldrChunks
#else
foldrChunks f z b = foldr f z (BL.toChunks b)
#endif
{-# INLINE foldrChunks #-}

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- TODO: Should this create the list in chunks, like unpackBytes does in 0.10?

-- | Unpack a strict 'B.Bytestring'
unpackStrict :: B.ByteString -> [Word8]
unpackStrict (BI.PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      go !p !q | p == q    = []
               | otherwise = let !x = BI.inlinePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                             in x : go (p `plusPtr` 1) q
{-# INLINE unpackStrict #-}

-- TODO: Should this create the list in chunks, like unpackBytes does in 0.10?

-- | Unpack a strict 'B.Bytestring', pretending the bytes are chars.
unpackStrict8 :: B.ByteString -> String
unpackStrict8 (BI.PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      go !p !q | p == q    = []
               | otherwise = let !x = BI.inlinePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                             in w2c x : go (p `plusPtr` 1) q
{-# INLINE unpackStrict8 #-}


-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> B.ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO B.ByteString
create l f = do
    fp <- mallocPlainForeignPtrBytes l
    withForeignPtr fp $ \p -> f p
    return $! BI.PS fp 0 l
{-# INLINE create #-}
