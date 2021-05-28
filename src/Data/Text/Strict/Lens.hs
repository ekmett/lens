{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Strict.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Text.Strict.Lens
  ( packed, unpacked
  , builder
  , text
  , utf8
  , _Text
  , pattern Text
  ) where

import Control.Lens.Type
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Iso
import Control.Lens.Prism
import Control.Lens.Review
import Control.Lens.Setter
import Control.Lens.Traversal
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.Text as Strict
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens

-- | This isomorphism can be used to 'pack' (or 'unpack') strict 'Text'.
--
--
-- >>> "hello"^.packed -- :: Text
-- "hello"
--
-- @
-- 'pack' x ≡ x '^.' 'packed'
-- 'unpack' x ≡ x '^.' 'from' 'packed'
-- 'packed' ≡ 'from' 'unpacked'
-- 'packed' ≡ 'iso' 'pack' 'unpack'
-- @
packed :: Iso' String Text
packed = iso Strict.pack Strict.unpack
{-# INLINE packed #-}

-- | This isomorphism can be used to 'unpack' (or 'pack') lazy 'Text'.
--
-- >>> "hello"^.unpacked -- :: String
-- "hello"
--
-- This 'Iso' is provided for notational convenience rather than out of great need, since
--
-- @
-- 'unpacked' ≡ 'from' 'packed'
-- @
--
-- @
-- 'pack' x ≡ x '^.' 'from' 'unpacked'
-- 'unpack' x ≡ x '^.' 'packed'
-- 'unpacked' ≡ 'iso' 'unpack' 'pack'
-- @
unpacked :: Iso' Text String
unpacked = iso Strict.unpack Strict.pack
{-# INLINE unpacked #-}

-- | This is an alias for 'unpacked' that makes it more obvious how to use it with '#'
--
-- >> _Text # "hello" -- :: Text
-- "hello"
_Text :: Iso' Text String
_Text = unpacked
{-# INLINE _Text #-}

-- | Convert between strict 'Text' and 'Builder' .
--
-- @
-- 'fromText' x ≡ x '^.' 'builder'
-- 'toStrict' ('toLazyText' x) ≡ x '^.' 'from' 'builder'
-- @
builder :: Iso' Text Builder
builder = iso Builder.fromText (toStrict . Builder.toLazyText)
{-# INLINE builder #-}

-- | Traverse the individual characters in strict 'Text'.
--
-- >>> anyOf text (=='o') "hello"
-- True
--
-- When the type is unambiguous, you can also use the more general 'each'.
--
-- @
-- 'text' ≡ 'unpacked' . 'traversed'
-- 'text' ≡ 'each'
-- @
--
-- Note that when just using this as a 'Setter', @'setting' 'Data.Text.map'@ can
-- be more efficient.
text :: IndexedTraversal' Int Text Char
text = unpacked . traversed
{-# INLINE [0] text #-}

{-# RULES
"strict text -> map"    text = sets Strict.map        :: ASetter' Text Char;
"strict text -> imap"   text = isets imapStrict       :: AnIndexedSetter' Int Text Char;
"strict text -> foldr"  text = foldring Strict.foldr  :: Getting (Endo r) Text Char;
"strict text -> ifoldr" text = ifoldring ifoldrStrict :: IndexedGetting Int (Endo r) Text Char;
 #-}

imapStrict :: (Int -> Char -> Char) -> Text -> Text
imapStrict f = snd . Strict.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapStrict #-}

ifoldrStrict :: (Int -> Char -> a -> a) -> a -> Text -> a
ifoldrStrict f z xs = Strict.foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldrStrict #-}

-- | Encode\/Decode a strict 'Text' to\/from strict 'ByteString', via UTF-8.
--
-- >>> utf8 # "☃"
-- "\226\152\131"
utf8 :: Prism' ByteString Text
utf8 = prism' encodeUtf8 (preview _Right . decodeUtf8')
{-# INLINE utf8 #-}

pattern Text :: String -> Text
pattern Text a <- (view _Text -> a) where
  Text a = review _Text a
