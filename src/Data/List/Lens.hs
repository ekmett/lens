{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
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
_head :: SimpleIndexedTraversal Int [a] a
_head = index $ \f aas -> case aas of
  (a:as) -> (:as) <$> f (0 :: Int) a
  _      -> pure aas
{-# INLINE _head #-}

-- | A 'Traversal' reading and writing to the 'tail' of a /non-empty/ list
--
-- >>> _tail .~ [3,4,5] $ [1,2]
-- [1,3,4,5]
_tail :: Simple Traversal [a] [a]
_tail f (a:as) = (a:) <$> f as
_tail _ as     = pure as
{-# INLINE _tail #-}

-- | A 'Traversal' reading and writing to the last element of a /non-empty/ list
--
-- >>> [1,2]^?!_last
-- 2
_last :: SimpleIndexedTraversal Int [a] a
_last = index $ \f aas -> case aas of
  []     -> pure aas
  (a:as) -> let go !n b []  = return <$> f n b
                go !n b (c:cs) = (b:) <$> go (n + 1) c cs
            in go (0 :: Int) a as
{-# INLINE _last #-}

-- | A 'Traversal' reading and replacing all but the a last element of a /non-empty/ list
--
-- >>> [1,2,3,4]^?!_init
-- [1,2,3]
_init :: Simple Traversal [a] [a]
_init _ [] = pure []
_init f as = (++ [Prelude.last as]) <$> f (Prelude.init as)
{-# INLINE _init #-}
