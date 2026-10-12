{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "lens-common.h"

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
  ( traversedStrictTree, traversedStrictTree8
  , traversedLazy, traversedLazy8
  ) where

import Prelude ()

import Control.Lens.Type
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Indexed
import Control.Lens.Internal.Prelude
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
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
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

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString'
traversedLazy :: IndexedTraversal' Int64 BL.ByteString Word8
traversedLazy pafb = \lbs -> BL.foldrChunks go (\_ -> pure BL.empty) lbs 0
  where
  go c fcs acc = BL.append . BL.fromStrict
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

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString' pretending the bytes are chars.
traversedLazy8 :: IndexedTraversal' Int64 BL.ByteString Char
traversedLazy8 pafb = \lbs -> BL.foldrChunks go (\_ -> pure BL.empty) lbs 0
  where
  go c fcs acc = BL.append . BL.fromStrict
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
#if MIN_VERSION_bytestring(0,11,0)
    return $! BI.BS fp l
#else
    return $! BI.PS fp 0 l
#endif
{-# INLINE create #-}
