{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lazy.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Text.Lazy.Lens
  ( packed, unpacked
  , prefixed, suffixed
  , _Text
  , text
  , builder
  , utf8
#if __GLASGOW_HASKELL__ >= 710
  , pattern Text
#endif
  ) where

import Control.Lens.Type
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Iso
import Control.Lens.Prism
#if __GLASGOW_HASKELL__ >= 710
import Control.Lens.Review
#endif
import Control.Lens.Setter
import Control.Lens.Traversal
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Encoding

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> import qualified Data.ByteString.Lazy as ByteString

-- | This isomorphism can be used to 'pack' (or 'unpack') lazy 'Text'.
--
-- >>> "hello"^.packed -- :: Text
-- "hello"
--
-- @
-- 'pack' x ≡ x '^.' 'packed'
-- 'unpack' x ≡ x '^.' 'from' 'packed'
-- 'packed' ≡ 'from' 'unpacked'
-- @
packed :: Iso' String Text
packed = iso Text.pack Text.unpack
{-# INLINE packed #-}

-- | This isomorphism can be used to 'unpack' (or 'pack') lazy 'Text'.
--
-- >>> "hello"^.unpacked -- :: String
-- "hello"
--
-- @
-- 'pack' x ≡ x '^.' 'from' 'unpacked'
-- 'unpack' x ≡ x '^.' 'packed'
-- @
--
-- This 'Iso' is provided for notational convenience rather than out of great need, since
--
-- @
-- 'unpacked' ≡ 'from' 'packed'
-- @
unpacked :: Iso' Text String
unpacked = iso Text.unpack Text.pack
{-# INLINE unpacked #-}

-- | A 'Prism' stripping a prefix from a text when used as a 'Traversal', or
-- prepending that prefix when run backwards:
--
-- >>> "preview" ^? prefixed "pre"
-- Just "view"
--
-- >>> "review" ^? prefixed "pre"
-- Nothing
--
-- >>> prefixed "pre" # "amble"
-- "preamble"
prefixed :: Text -> Prism' Text Text
prefixed p = prism' (p <>) (Text.stripPrefix p)

-- | A 'Prism' stripping a suffix from a text when used as a 'Traversal', or
-- appending that suffix when run backwards:
--
-- >>> "review" ^? suffixed "view"
-- Just "re"
--
-- >>> "review" ^? suffixed "tire"
-- Nothing
--
-- >>> suffixed ".o" # "hello"
-- "hello.o"
suffixed :: Text -> Prism' Text Text
suffixed q = prism' (<> q) (Text.stripSuffix q)

-- | This is an alias for 'unpacked' that makes it clearer how to use it with @('#')@.
--
-- @
-- '_Text' = 'from' 'packed'
-- @
--
-- >>> _Text # "hello" -- :: Text
-- "hello"
_Text :: Iso' Text String
_Text = from packed
{-# INLINE _Text #-}

-- | Convert between lazy 'Text' and 'Builder' .
--
-- @
-- 'fromLazyText' x ≡ x '^.' 'builder'
-- 'toLazyText' x ≡ x '^.' 'from' 'builder'
-- @
builder :: Iso' Text Builder
builder = iso Builder.fromLazyText Builder.toLazyText
{-# INLINE builder #-}

-- | Traverse the individual characters in a 'Text'.
--
-- >>> anyOf text (=='c') "chello"
-- True
--
-- @
-- 'text' = 'unpacked' . 'traversed'
-- @
--
-- When the type is unambiguous, you can also use the more general 'each'.
--
-- @
-- 'text' ≡ 'each'
-- @
--
-- Note that when just using this as a 'Setter', @'setting' 'Data.Text.Lazy.map'@
-- can be more efficient.
text :: IndexedTraversal' Int Text Char
text = unpacked . traversed
{-# INLINE [0] text #-}

{-# RULES
"lazy text -> map"    text = sets Text.map        :: ASetter' Text Char;
"lazy text -> imap"   text = isets imapLazy       :: AnIndexedSetter' Int Text Char;
"lazy text -> foldr"  text = foldring Text.foldr  :: Getting (Endo r) Text Char;
"lazy text -> ifoldr" text = ifoldring ifoldrLazy :: IndexedGetting Int (Endo r) Text Char;
 #-}

imapLazy :: (Int -> Char -> Char) -> Text -> Text
imapLazy f = snd . Text.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapLazy #-}

ifoldrLazy :: (Int -> Char -> a -> a) -> a -> Text -> a
ifoldrLazy f z xs = Text.foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldrLazy #-}

-- | Encode\/Decode a lazy 'Text' to\/from lazy 'ByteString', via UTF-8.
--
-- Note: This function does not decode lazily, as it must consume the entire
-- input before deciding whether or not it fails.
--
-- >>> ByteString.unpack (utf8 # "☃")
-- [226,152,131]
utf8 :: Prism' ByteString Text
utf8 = prism' encodeUtf8 (preview _Right . decodeUtf8')
{-# INLINE utf8 #-}

#if __GLASGOW_HASKELL__ >= 710
# if __GLASGOW_HASKELL__ >= 800
pattern Text :: String -> Text
# endif
pattern Text a <- (view _Text -> a) where
  Text a = review _Text a
#endif
