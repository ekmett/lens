module Data.ByteString.Lens
  ( TraverseByteString(..)
  ) where

import Control.Lens
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Word (Word8)

-- | Provides ad hoc overloading for 'traverseByteString'
class TraverseByteString t where
  -- | Traverse the individual bytes in a 'ByteString'
  --
  -- > anyOf traverseByteString (==0x80) :: TraverseByteString b => b -> Bool
  traverseByteString :: Simple Traversal t Word8

instance TraverseByteString Strict.ByteString where
  traverseByteString f = fmap Strict.pack . traverse f . Strict.unpack

instance TraverseByteString Lazy.ByteString where
  traverseByteString f = fmap Lazy.pack . traverse f . Lazy.unpack
