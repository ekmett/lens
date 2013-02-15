{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Strict.Lens
-- Copyright   :  (C) 2012-2013 Edward Kmett
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
  ( unpackStrict, traversedStrict, traversedStrictTree
  , unpackStrict8, traversedStrict8, traversedStrictTree8
  , unpackLazy, traversedLazy
  , unpackLazy8, traversedLazy8
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.ByteString.Internal      as BI
import Data.Bits
import Data.Char
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Storable
#if MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
#else
import Foreign.ForeignPtr
#endif
import GHC.Base (unsafeChr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)

-- | Traverse a strict 'B.ByteString' from left to right in a biased fashion.
traversedStrict :: Int -> IndexedTraversal' Int B.ByteString Word8
traversedStrict i0 pafb (BI.PS fp off len) =
  let p = unsafeForeignPtrToPtr fp
   in fmap (rebuild len) (go i0 (p `plusPtr` off) (p `plusPtr` (off+len)))
 where
   rebuild n = \xs -> unsafeCreate n $ \p -> go2 p xs
   go2 !p (x:xs) = poke p x >> go2 (p `plusPtr` 1) xs
   go2 _  []     = return ()
   -- TODO: use a balanced tree (up to some grain size)
   go !i !p !q
     | p == q = pure []
     | otherwise = let !x = BI.inlinePerformIO $ do
                              x' <- peek p
                              touchForeignPtr fp
                              return x'
                   in (:) <$> indexed pafb (i :: Int) x <*> go (i + 1) (p `plusPtr` 1) q
{-# INLINE traversedStrict #-}

-- | Traverse a strict 'B.ByteString' from left to right in a biased fashion
-- pretending the bytes are characters.
traversedStrict8 :: Int -> IndexedTraversal' Int B.ByteString Char
traversedStrict8 i0 pafb (BI.PS fp off len) =
  let p = unsafeForeignPtrToPtr fp
   in fmap (rebuild len) (go i0 (p `plusPtr` off) (p `plusPtr` (off+len)))
 where
   rebuild n = \xs -> unsafeCreate n $ \p -> go2 p xs
   go2 !p (x:xs) = poke p (c2w x) >> go2 (p `plusPtr` 1) xs
   go2 _  []     = return ()
   -- TODO: use a balanced tree (up to some grain size)
   go !i !p !q
     | p == q = pure []
     | otherwise = let !x = BI.inlinePerformIO $ do
                              x' <- peek p
                              touchForeignPtr fp
                              return x'
                   in (:) <$> indexed pafb (i :: Int) (w2c x) <*> go (i + 1) (p `plusPtr` 1) q
{-# INLINE traversedStrict8 #-}

grain :: Int
grain = 32
{-# INLINE grain #-}

-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a balanced tree with biased runs of
-- elements at the leaves.
traversedStrictTree :: Int -> IndexedTraversal' Int B.ByteString Word8
traversedStrictTree i0 pafb (BI.PS fp off len) = rebuild len <$> go (unsafeForeignPtrToPtr fp `plusPtr` (off - i0)) i0 (i0 + len)
 where
   rebuild n f = unsafeCreate n $ \q -> f $! (q `plusPtr` (off - i0))
   go !p !i !j
     | i + grain < j, k <- i + shiftR (j - i) 1 = (\l r q -> l q >> r q) <$> go p i k <*> go p k j
     | otherwise = run p i j
   run !p !i !j
     | i == j    = pure (\_ -> return ())
     | otherwise = let !x = BI.inlinePerformIO $ do
                          x' <- peekByteOff p i
                          touchForeignPtr fp
                          return x'
                   in (\y ys !q -> pokeByteOff q i y >> ys q) <$> indexed pafb (i :: Int) x <*> run p (i + 1) j
{-# INLINE traversedStrictTree #-}

-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a balanced tree with biased runs of
-- elements at the leaves, pretending the bytes are chars.
traversedStrictTree8 :: Int -> IndexedTraversal' Int B.ByteString Char
traversedStrictTree8 i0 pafb (BI.PS fp off len) = rebuild len <$> go i0 (i0 + len)
 where
   p = unsafeForeignPtrToPtr fp `plusPtr` (off - i0)
   rebuild n f = unsafeCreate n $ \q -> f (q `plusPtr` (off - i0))
   go !i !j
     | i + grain < j, k <- i + shiftR (j - i) 1 = (\l r q -> l q >> r q) <$> go i k <*> go k j
     | otherwise = run i j
   run !i !j
     | i == j    = pure (\_ -> return ())
     | otherwise = let !x = BI.inlinePerformIO $ do
                          x' <- peekByteOff p i
                          touchForeignPtr fp
                          return x'
                   in (\y ys q -> poke (q `plusPtr` i) (c2w y) >> ys q) <$> indexed pafb (i :: Int) (w2c x) <*> run (i + 1) j
{-# INLINE traversedStrictTree8 #-}

-- | Unpack a lazy 'Bytestring'
unpackLazy :: BL.ByteString -> [Word8]
unpackLazy = BL.unpack
{-# INLINE unpackLazy #-}

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString'
traversedLazy :: IndexedTraversal' Int64 BL.ByteString Word8
traversedLazy pafb = go 0 where
  go _ BLI.Empty        = pure BLI.Empty
  go i (BLI.Chunk b bs) = BLI.Chunk <$> reindexed (fromIntegral :: Int -> Int64) (traversedStrictTree (fromIntegral i)) pafb b <*> go i' bs
    where !i' = i + B.length b
{-# INLINE traversedLazy #-}


-- | Unpack a lazy 'BL.ByteString' pretending the bytes are chars.
unpackLazy8 :: BL.ByteString -> String
unpackLazy8 = BL8.unpack
{-# INLINE unpackLazy8 #-}

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString' pretending the bytes are chars.
traversedLazy8 :: IndexedTraversal' Int64 BL.ByteString Char
traversedLazy8 pafb = go 0 where
  go _ BLI.Empty = pure BLI.Empty
  go i (BLI.Chunk b bs) = BLI.Chunk <$> reindexed (fromIntegral :: Int -> Int64) (traversedStrictTree8 (fromIntegral i)) pafb b <*> go i' bs
    where !i' = i + B.length b
{-# INLINE traversedLazy8 #-}

------------------------------------------------------------------------------
-- ByteString guts
------------------------------------------------------------------------------

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
