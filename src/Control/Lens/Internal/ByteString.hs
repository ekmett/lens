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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Strict.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Lens.Type
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Indexed
import Control.Lens.Setter
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Unsafe        as BU
import Data.Bits
import Data.Char
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Monoid
import Foreign.Ptr
import Foreign.Storable
#if MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr
#elif MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Safe
#if !MIN_VERSION_bytestring(0,10,4)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#endif
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
                   in (\y ys q -> pokeByteOff q i y >> ys q) <$> indexed pafb (i :: Int) x <*> run (i + 1) j
{-# INLINE [0] traversedStrictTree #-}

{-# RULES
"bytes -> map"    traversedStrictTree = sets B.map        :: ASetter' B.ByteString Word8;
"bytes -> imap"   traversedStrictTree = isets imapB       :: AnIndexedSetter' Int B.ByteString Word8;
"bytes -> foldr"  traversedStrictTree = foldring B.foldr  :: Getting (Endo r) B.ByteString Word8;
"bytes -> ifoldr" traversedStrictTree = ifoldring ifoldrB :: IndexedGetting Int (Endo r) B.ByteString Word8;
 #-}

imapB :: (Int -> Word8 -> Word8) -> B.ByteString -> B.ByteString
imapB f = snd . B.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapB #-}

ifoldrB :: (Int -> Word8 -> a -> a) -> a -> B.ByteString -> a
ifoldrB f z xs = B.foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldrB #-}

-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a balanced tree with biased runs of
-- elements at the leaves, pretending the bytes are chars.
traversedStrictTree8 :: IndexedTraversal' Int B.ByteString Char
traversedStrictTree8 pafb bs = unsafeCreate len <$> go 0 len
 where
   len = B.length bs
   go !i !j
     | i + grain < j    = let k = i + shiftR (j - i) 1
                          in (\l r q -> l q >> r q) <$> go i k <*> go k j
     | otherwise        = run i j
   run !i !j
     | i == j           = pure (\_ -> return ())
     | otherwise        = let !x = BU.unsafeIndex bs i
                          in (\y ys q -> pokeByteOff q i (c2w y) >> ys q)
                         <$> indexed pafb (i :: Int) (w2c x)
                         <*> run (i + 1) j
{-# INLINE [0] traversedStrictTree8 #-}

{-# RULES
"chars -> map"    traversedStrictTree8 = sets B8.map        :: ASetter' B.ByteString Char;
"chars -> imap"   traversedStrictTree8 = isets imapB8       :: AnIndexedSetter' Int B.ByteString Char;
"chars -> foldr"  traversedStrictTree8 = foldring B8.foldr  :: Getting (Endo r) B.ByteString Char;
"chars -> ifoldr" traversedStrictTree8 = ifoldring ifoldrB8 :: IndexedGetting Int (Endo r) B.ByteString Char;
 #-}

imapB8 :: (Int -> Char -> Char) -> B.ByteString -> B.ByteString
imapB8 f = snd . B8.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapB8 #-}

ifoldrB8 :: (Int -> Char -> a -> a) -> a -> B.ByteString -> a
ifoldrB8 f z xs = B8.foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldrB8 #-}

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
{-# INLINE [1] traversedLazy #-}

{-# RULES
  "sets lazy bytestring"
    traversedLazy = sets BL.map :: ASetter' BL.ByteString Word8;
  "isets lazy bytestring"
    traversedLazy = isets imapBL :: AnIndexedSetter' Int BL.ByteString Word8;
  "gets lazy bytestring"
    traversedLazy = foldring BL.foldr :: Getting (Endo r) BL.ByteString Word8;
  "igets lazy bytestring"
    traversedLazy = ifoldring ifoldrBL :: IndexedGetting Int (Endo r) BL.ByteString Word8;
 #-}

imapBL :: (Int -> Word8 -> Word8) -> BL.ByteString -> BL.ByteString
imapBL f = snd . BL.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapBL #-}

ifoldrBL :: (Int -> Word8 -> a -> a) -> a -> BL.ByteString -> a
ifoldrBL f z xs = BL.foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldrBL #-}

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
{-# INLINE [1] traversedLazy8 #-}

{-# RULES
  "sets lazy bytestring"
    traversedLazy8 = sets BL8.map :: ASetter' BL8.ByteString Char;
  "isets lazy bytestring"
    traversedLazy8 = isets imapBL8 :: AnIndexedSetter' Int BL8.ByteString Char;
  "gets lazy bytestring"
    traversedLazy8 = foldring BL8.foldr :: Getting (Endo r) BL8.ByteString Char;
  "igets lazy bytestring"
    traversedLazy8 = ifoldring ifoldrBL8 :: IndexedGetting Int (Endo r) BL8.ByteString Char;
 #-}

imapBL8 :: (Int -> Char -> Char) -> BL8.ByteString -> BL8.ByteString
imapBL8 f = snd . BL8.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapBL8 #-}

ifoldrBL8 :: (Int -> Char -> a -> a) -> a -> BL8.ByteString -> a
ifoldrBL8 f z xs = BL8.foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldrBL8 #-}

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

-- | Unpack a strict 'B.Bytestring'
unpackStrict :: B.ByteString -> [Word8]
#if MIN_VERSION_bytestring(0,10,4)
unpackStrict = B.unpack
#else
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
#endif
{-# INLINE unpackStrict #-}

-- | Unpack a strict 'B.Bytestring', pretending the bytes are chars.
unpackStrict8 :: B.ByteString -> String
#if MIN_VERSION_bytestring(0,10,4)
unpackStrict8 = B8.unpack
#else
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
#endif
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
