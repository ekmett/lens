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
  ( traverseHead
  , traverseTail
  , traverseInit
  , traverseLast
  ) where

import Control.Lens

-- The traversal for reading and writing to the head of a list
--
-- > traverseHead = traverseValueAtMin
-- > traverseHead = traverseElementAt 0 -- but is more efficient
--
-- | > traverseHead :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseHead :: SimpleTraversal [a] a
traverseHead _ [] = pure []
traverseHead f (a:as) = (:as) <$> f a
{-# INLINE traverseHead #-}

-- | Traversal for editing the tail of a list.
--
-- > traverseTail :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseTail :: SimpleTraversal [a] a
traverseTail _ [] = pure []
traverseTail f (a:as) = (a:) <$> traverse f as
{-# INLINE traverseTail #-}

-- | Traverse the last element in a list.
--
-- > traverseLast = traverseValueAtMax
--
-- > traverseLast :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseLast :: SimpleTraversal [a] a
traverseLast _ []     = pure []
traverseLast f [a]    = return <$> f a
traverseLast f (a:as) = (a:) <$> traverseLast f as
{-# INLINE traverseLast #-}

-- The traversal for reading and writing to the tail of a list

-- | Traverse all but the last element of a list
--
-- > traverseInit :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseInit :: SimpleTraversal [a] a
traverseInit _ [] = pure []
traverseInit f as = (++ [Prelude.last as]) <$> traverse f (Prelude.init as)
{-# INLINE traverseInit #-}
