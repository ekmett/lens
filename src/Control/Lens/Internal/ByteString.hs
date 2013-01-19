{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Strict.Lens
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module spends a lot of time fiddling around with 'ByteString' internals
-- to work around <http://hackage.haskell.org/trac/ghc/ticket/7556> only older
-- Haskell Platforms and to improve constant factors in our performance.
----------------------------------------------------------------------------
module Control.Lens.Internal.ByteString
  ( unpackStrict, traversedStrict
  , unpackStrict8, traversedStrict8
  , unpackLazy, traversedLazy
  , unpackLazy8, traversedLazy8
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.ByteString.Internal      as BI
import Data.Char
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import GHC.Base (unsafeChr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)


-- Takes an argument for the initial index
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

-- Takes an argument for the initial index
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

unpackLazy :: BL.ByteString -> [Word8]
unpackLazy = BL.unpack
{-# INLINE unpackLazy #-}

traversedLazy :: IndexedTraversal' Int64 BL.ByteString Word8
traversedLazy = iso unpackLazy BL.pack . traversed64
{-# INLINE traversedLazy #-}

unpackLazy8 :: BL.ByteString -> [Char]
unpackLazy8 = BL8.unpack
{-# INLINE unpackLazy8 #-}

traversedLazy8 :: IndexedTraversal' Int64 BL.ByteString Char
traversedLazy8 = iso unpackLazy8 BL8.pack . traversed64
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
