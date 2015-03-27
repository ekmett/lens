{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Lens
-- Copyright   :  (C) 2012-15 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Traversals for manipulating parts of a list.
--
----------------------------------------------------------------------------
module Data.List.Lens
  ( prefixed
  , suffixed
  , stripSuffix
  ) where

import Control.Monad (guard)
import Control.Lens
import Data.List

#if !MIN_VERSION_base(4,8,0)
import Data.Functor
#endif

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- | A 'Prism' stripping a prefix from a list when used as a 'Traversal', or
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
prefixed :: Eq a => [a] -> Prism' [a] [a]
prefixed ps = prism' (ps ++) (stripPrefix ps)
{-# INLINE prefixed #-}

-- | A 'Prism' stripping a suffix from a list when used as a 'Traversal', or
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
suffixed :: Eq a => [a] -> Prism' [a] [a]
suffixed qs = prism' (++ qs) (stripSuffix qs)
{-# INLINE suffixed #-}

------------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------------

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix qs xs0 = go xs0 zs
  where
    zs = drp qs xs0
    drp (_:ps) (_:xs) = drp ps xs
    drp [] xs = xs
    drp _  [] = []
    go (_:xs) (_:ys) = go xs ys
    go xs [] = zipWith const xs0 zs <$ guard (xs == qs)
    go [] _  = Nothing -- impossible
{-# INLINE stripSuffix #-}
