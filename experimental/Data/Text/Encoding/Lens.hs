{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lens.Encoding
-- Copyright   :  (C) 2013 Michael Thompson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Text.Encoding.Lens where

import Control.Lens
import Data.ByteString
import Data.Text
import Data.Text.Encoding as T

utf8 :: Iso' Text ByteString
utf8 = iso encodeUtf8 decodeUtf8
{-#INLINE utf8#-}

utf16LE :: Iso' Text ByteString
utf16LE = iso encodeUtf16LE decodeUtf16LE
{-#INLINE utf16LE#-}

utf16BE :: Iso' Text ByteString
utf16BE = iso encodeUtf16BE decodeUtf16BE
{-#INLINE utf16BE#-}

utf32LE :: Iso' Text ByteString
utf32LE = iso encodeUtf32LE decodeUtf32LE
{-#INLINE utf32LE#-}

utf32BE :: Iso' Text ByteString
utf32BE = iso encodeUtf32BE decodeUtf32BE
{-#INLINE utf32BE#-}
