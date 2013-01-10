{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Lens
-- Copyright   :  (C) 2012 Edward Kmett
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

import Control.Applicative
import Control.Lens
import Data.List

-- $setup
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- | A 'Traversal' reading and replacing all but the a last element of a /non-empty/ list
--
-- >>> [a,b,c,d]^?_init
-- Just [a,b,c]
--
-- >>> []^?_init
-- Nothing
--
-- >>> [a,b] & _init .~ [c,d,e]
-- [c,d,e,b]
--
-- >>> [] & _init .~ [a,b]
-- []
--
-- >>> [a,b,c,d] & _init.traverse %~ f
-- [f a,f b,f c,d]
--
-- >>> [1,2,3]^?_init
-- Just [1,2]
--
-- >>> [1,2,3,4]^?!_init
-- [1,2,3]
--
-- >>> "hello"^._init
-- "hell"
--
-- >>> ""^._init
-- ""
_init :: Traversal' [a] [a]
_init _ [] = pure []
_init f as = (++ [Prelude.last as]) <$> f (Prelude.init as)
{-# INLINE _init #-}

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
strippingPrefix ps = prism (ps ++) $ \xs -> case stripPrefix ps xs of
  Nothing  -> Left xs
  Just xs' -> Right xs'
{-# INLINE strippingPrefix #-}
