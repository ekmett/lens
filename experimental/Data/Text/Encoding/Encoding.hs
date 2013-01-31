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

import           Control.Lens             
import qualified Data.ByteString          as B
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T


utf8 :: Iso' T.Text B.ByteString
utf8 = iso T.encodeUtf8 T.decodeUtf8
{-#INLINE utf8#-}

utf16LE :: Iso' T.Text B.ByteString
utf16LE = iso T.encodeUtf16LE T.decodeUtf16LE
{-#INLINE utf16LE#-}

utf16BE :: Iso' T.Text B.ByteString
utf16BE = iso T.encodeUtf16BE T.decodeUtf16BE
{-#INLINE utf16BE#-}

utf32LE :: Iso' T.Text B.ByteString
utf32LE = iso T.encodeUtf32LE T.decodeUtf32LE
{-#INLINE utf32LE#-}

utf32BE :: Iso' T.Text B.ByteString
utf32BE = iso T.encodeUtf32BE T.decodeUtf32BE
{-#INLINE utf32BE#-}