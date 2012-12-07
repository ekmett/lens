{-# LANGUAGE LiberalTypeSynonyms #-}
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
  (
    _head
  , _tail
  , _last
  , _init
  , strippingPrefix
  ) where

import Control.Applicative
import Control.Lens
import Data.List

-- $setup
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- | A 'Traversal' reading and writing to the 'head' of a /non-empty/ list.
--
-- >>> [a,b,c]^? _head
-- Just a
--
-- >>> [a,b,c] & _head .~ d
-- [d,b,c]
--
-- >>> [a,b,c] & _head %~ f
-- [f a,b,c]
--
-- >>> [] & _head %~ f
-- []
--
-- >>> [1,2,3]^?!_head
-- 1
--
-- >>> []^?_head
-- Nothing
--
-- >>> [1,2]^?_head
-- Just 1
--
-- >>> [] & _head .~ 1
-- []
--
-- >>> [0] & _head .~ 2
-- [2]
--
-- >>> [0,1] & _head .~ 2
-- [2,1]
--
--
_head :: SimpleIndexedTraversal Int [a] a
_head = indexed $ \f aas -> case aas of
  (a:as) -> (:as) <$> f (0 :: Int) a
  _      -> pure aas
{-# INLINE _head #-}

-- | A 'Traversal' reading and writing to the 'tail' of a /non-empty/ list
--
-- >>> [a,b] & _tail .~ [c,d,e]
-- [a,c,d,e]
--
-- >>> [] & _tail .~ [a,b]
-- []
--
-- >>> [a,b,c,d,e] & _tail.traverse %~ f
-- [a,f b,f c,f d,f e]
--
-- >>> [1,2] & _tail .~ [3,4,5]
-- [1,3,4,5]
--
-- >>> [] & _tail .~ [1,2]
-- []
--
-- >>> [a,b,c]^?_tail
-- Just [b,c]
--
-- >>> [1,2]^?!_tail
-- [2]
--
-- >>> "hello"^._tail
-- "ello"
--
-- >>> ""^._tail
-- ""
_tail :: Simple Traversal [a] [a]
_tail f (a:as) = (a:) <$> f as
_tail _ as     = pure as
{-# INLINE _tail #-}

-- | A 'Traversal' reading and writing to the last element of a /non-empty/ list
--
-- >>> [a,b,c]^?!_last
-- c
--
-- >>> []^?_last
-- Nothing
--
-- >>> [a,b,c] & _last %~ f
-- [a,b,f c]
--
-- >>> [1,2]^?_last
-- Just 2
--
-- >>> [] & _last .~ 1
-- []
--
-- >>> [0] & _last .~ 2
-- [2]
--
-- >>> [0,1] & _last .~ 2
-- [0,2]
_last :: SimpleIndexedTraversal Int [a] a
_last = indexed $ \f aas -> case aas of
  []     -> pure aas
  (a:as) -> let go n b []  = return <$> f n b
                go n b (c:cs) = (b:) <$> (go $! n + 1) c cs
            in go (0 :: Int) a as
{-# INLINE _last #-}

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
_init :: Simple Traversal [a] [a]
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
-- >>> "amble"^.remit (strippingPrefix "pre")
-- "preamble"
strippingPrefix :: Eq a => [a] -> Simple Prism [a] [a]
strippingPrefix ps = prism (ps ++) $ \xs -> case stripPrefix ps xs of
  Nothing  -> Left xs
  Just xs' -> Right xs'
