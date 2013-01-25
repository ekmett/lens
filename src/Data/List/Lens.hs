{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Traversals for manipulating parts of a list.
--
----------------------------------------------------------------------------
module Data.List.Lens
  ( strippingPrefix
  , strippingSuffix
  ) where

import Control.Monad (guard)
import Control.Lens
import Data.Functor
import Data.List

-- $setup
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- | A 'Prism' stripping a prefix from a list when used as a 'Traversal', or
-- prepending that prefix when run backwards:
--
-- >>> "preview" ^? strippingPrefix "pre"
-- Just "view"
--
-- >>> "review" ^? strippingPrefix "pre"
-- Nothing
--
-- >>> "amble"^.re (strippingPrefix "pre")
-- "preamble"
strippingPrefix :: Eq a => [a] -> Prism' [a] [a]
strippingPrefix ps = prism' (ps ++) (stripPrefix ps)
{-# INLINE strippingPrefix #-}

-- | A 'Prism' stripping a suffix from a list when used as a 'Traversal', or
-- prepending that prefix when run backwards:
--
-- >>> "review" ^? strippingSuffix "view"
-- Just "re"
--
-- >>> "review" ^? strippingSuffix "tire"
-- Nothing
--
-- >>> "hello"^.re (strippingSuffix ".o")
-- "hello.o"
strippingSuffix :: Eq a => [a] -> Prism' [a] [a]
strippingSuffix qs = prism' (++ qs) (stripSuffix qs)
{-# INLINE strippingSuffix #-}

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
