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
  ) where

import Control.Lens
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
