{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Strict.Lens
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.ByteString.Strict.Lens
  ( packedBytes, bytes
  , packedChars, chars
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString as B
import Data.ByteString as Words
import Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as BI
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)

-- $setup
-- >>> import Control.Lens

-- | 'Data.ByteString.pack' (or 'Data.ByteString.unpack') a list of bytes into a 'ByteString'
--
-- @'Data.ByteString.pack' x = x '^.' 'packedBytes'@
--
-- @'Data.ByteString.unpack' x = x '^.' 'from' 'packedBytes'@
packedBytes :: Iso' [Word8] ByteString
packedBytes = iso B.pack unpack09
{-# INLINE packedBytes #-}

-- | Traverse each 'Word8' in a 'ByteString'.
--
-- @'bytes' = 'from' 'packedBytes' '.' 'itraversed'@
--
-- @'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'@
bytes :: IndexedTraversal' Int B.ByteString Word8
bytes pafb (BI.PS fp off len) =
  let p = unsafeForeignPtrToPtr fp
   in fmap (rebuild len) (go 0 (p `plusPtr` off) (p `plusPtr` (off+len)))
 where
   rebuild n = \xs -> unsafeCreate n $ \p -> go2 p xs
   go2 !p (x:xs) = poke p x >> go2 (p `plusPtr` 1) xs
   go2 _  []     = return ()
   go !i !p !q
     | p == q = pure []
     | otherwise = let !x = BI.inlinePerformIO $ do
                              x' <- peek p
                              touchForeignPtr fp
                              return x'
                   in (:) <$> indexed pafb (i :: Int) x <*> go (i + 1) (p `plusPtr` 1) q
{-# INLINE bytes #-}

-- | 'Data.ByteString.Char8.pack' (or 'Data.ByteString.Char8.unpack') a list of characters into a 'ByteString'
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'Data.ByteString.Char8.pack' x = x '^.' 'packedChars'@
--
-- @'Data.ByteString.Char8.unpack' x = x '^.' 'from' 'packedChars'@
packedChars :: Iso' String ByteString
packedChars = iso Char8.pack Char8.unpack
{-# INLINE packedChars #-}

-- | Traverse the individual bytes in a 'ByteString' as characters.
--
-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between '\x00' and '\xff'.
--
-- @'chars' = 'from' 'packedChars' '.' 'traverse'@
--
-- @'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'@
chars :: IndexedTraversal' Int ByteString Char
chars = from packedChars . itraversed
{-# INLINE chars #-}

------------------------------------------------------------------------------
-- ByteString guts
------------------------------------------------------------------------------

-- Since we don't use foldr on its own except for creating a list, we can just
-- create a list directly. But maybe we should either (a) inline the whole
-- traversal or (b) create the list in chunks, like unpackBytes does in 0.10?
unpack09 :: B.ByteString -> [Word8]
unpack09 (BI.PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      go !p !q | p == q    = []
               | otherwise = let !x = BI.inlinePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                             in x : go (p `plusPtr` 1) q
{-# INLINE unpack09 #-}

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
