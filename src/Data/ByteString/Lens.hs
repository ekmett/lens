-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.ByteString.Lens
  ( IsByteString(..)
  , traverseByteString
  ) where

import Control.Lens
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Word (Word8)

-- | Provides ad hoc overloading for 'packedByteString'
class IsByteString t where
  -- | Pack or unpack a ByteString
  --
  -- > pack x = x^.packedByteString
  -- > unpack x = x^.from packedByteString
  packedByteString :: Simple Iso [Word8] t

instance IsByteString Strict.ByteString where
  packedByteString = iso Strict.pack Strict.unpack
  {-# INLINE packedByteString #-}

instance IsByteString Lazy.ByteString where
  packedByteString = iso Lazy.pack Lazy.unpack
  {-# INLINE packedByteString #-}

-- | Traverse the individual bytes in a 'ByteString'
--
-- > anyOf traverseByteString (==0x80) :: TraverseByteString b => b -> Bool
traverseByteString :: IsByteString t => Simple Traversal t Word8
traverseByteString = from packedByteString . traverse
{-# INLINE traverseByteString #-}

