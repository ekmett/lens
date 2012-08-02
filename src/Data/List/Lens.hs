{-# LANGUAGE LiberalTypeSynonyms #-}
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
  ( _head
  , _tail
  , _last
  , _init
  , interspersed
  , intercalated
  , traverseHead
  , traverseTail
  , traverseInit
  , traverseLast
  ) where

import Control.Applicative
import Control.Lens
import Data.List

-- | A lens reading and writing to the head of a _non-empty_ list
--
-- > ghci> [1,2,3]^._head
-- > 1
_head :: Simple Lens [a] a
_head _ [] = error "_head: empty list"
_head f (a:as) = (:as) <$> f a
{-# INLINE _head #-}

_tail :: Simple Lens [a] [a]
_tail _ [] = error "_tail: empty list"
_tail f (a:as) = (a:) <$> f as
{-# INLINE _tail #-}

_last :: Simple Lens [a] a
_last _ []     = error "_last: empty list"
_last f [a]    = return <$> f a
_last f (a:as) = (a:) <$> _last f as
{-# INLINE _last #-}

_init :: Simple Lens [a] [a]
_init _ [] = error "_init: empty list"
_init f as = (++ [Prelude.last as]) <$> f (Prelude.init as)
{-# INLINE _init #-}

-- | Obtain a version of the list with the supplied value interspersed.
--
-- > ghci> "abcde"^.interspersed ','
-- > "a,b,c,d,e"
--
-- > xs^.interspersed a = intersperse a xs
interspersed :: a -> Getter [a] [a]
interspersed = to . intersperse
{-# INLINE interspersed #-}

-- | Obtain a version of the list with the supplied value intercalated
intercalated :: [a] -> Getter [[a]] [a]
intercalated = to . intercalate
{-# INLINE intercalated #-}

-- | The traversal for reading and writing to the head of a list
--
-- > traverseHead = traverseValueAtMin
-- > traverseHead = traverseElementAt 0 -- but is more efficient
--
-- > traverseHead :: Applicative f => (a -> f a) -> [a] -> f [a]
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
