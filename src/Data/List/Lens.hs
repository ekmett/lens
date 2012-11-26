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
  -- * Partial Lenses
    _head
  , _tail
  , _last
  , _init
  ) where

import Control.Applicative
import Control.Lens


-- | A 'Traversal' reading and writing to the 'head' of a /non-empty/ list.
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
_head :: SimpleProjection [a] a
_head = projecting (:[]) $ \f aas -> case aas of
  (a:as) -> (:as) <$> f a
  _      -> pure aas
{-# INLINE _head #-}

-- | A 'Traversal' reading and writing to the 'tail' of a /non-empty/ list
--
-- >>> [1,2] & _tail .~ [3,4,5]
-- [1,3,4,5]
--
-- >>> [] & _tail .~ [1,2]
-- []
--
-- >>> [1,2,3]^?_tail
-- Just [2,3]
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
-- >>> [1,2,3]^?!_last
-- 3
--
-- >>> []^?_last
-- Nothing
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
_last :: SimpleProjection [a] a
_last = projecting (:[]) $ \f aas -> case aas of
  []     -> pure aas
  (a:as) -> let go b []  = return <$> f b
                go b (c:cs) = (b:) <$> go c cs
            in go a as
{-# INLINE _last #-}

-- | A 'Traversal' reading and replacing all but the a last element of a /non-empty/ list
--
-- >>> [1,2,3,4]^?!_init
-- [1,2,3]
--
-- >>> [1,2] & _init .~ [3,4,5]
-- [3,4,5,2]
--
-- >>> [] & _init .~ [1,2]
-- []
--
-- >>> [1,2,3]^?_init
-- Just [1,2]
--
-- >>> [1,2]^?!_init
-- [1]
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
