-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Traversals for manipulating parts of a list.
--
-- Additional optics for manipulating lists are present more
-- generically in this package.
--
-- The 'Control.Lens.At.Ixed' class allows traversing the element at a
-- specific list index.
--
-- >>> [0..10] ^? ix 4
-- Just 4
--
-- >>> [0..5] & ix 4 .~ 2
-- [0,1,2,3,2,5]
--
-- >>> [0..10] ^? ix 14
-- Nothing
--
-- >>> [0..5] & ix 14 .~ 2
-- [0,1,2,3,4,5]
--
-- The 'Control.Lens.Cons.Cons' and 'Control.Lens.Empty.AsEmpty'
-- classes provide 'Control.Lens.Prism.Prism's for list constructors.
--
-- >>> [1..10] ^? _Cons
-- Just (1,[2,3,4,5,6,7,8,9,10])
--
-- >>> [] ^? _Cons
-- Nothing
--
-- >>> [] ^? _Empty
-- Just ()
--
-- >>> _Cons # (1, _Empty # ()) :: [Int]
-- [1]
--
-- Additionally, 'Control.Lens.Cons.Snoc' provides a
-- 'Control.Lens.Prism.Prism' for accessing the end of a list. Note
-- that this 'Control.Lens.Prism.Prism' always will need to traverse
-- the whole list.
--
-- >>> [1..5] ^? _Snoc
-- Just ([1,2,3,4],5)
--
-- >>> _Snoc # ([1,2],5)
-- [1,2,5]
--
-- An instance of 'Control.Lens.Plated.Plated' allows for finding
-- locations in the list where a traversal matches.
--
-- >>> [Nothing, Just 7, Just 3, Nothing] & deep (ix 0 . _Just) +~ 10
-- [Nothing,Just 17,Just 3,Nothing]
--
-- An instance of 'Control.Lens.Iso.Reversing' provides an
-- 'Control.Lens.Iso.Iso' between a list and its reverse.
--
-- >>> "live" & reversed %~ ('d':)
-- "lived"
--
-- It's possible to work under a prefix or suffix of a list using
-- 'Control.Lens.Prism.Prefixed' and 'Control.Lens.Prism.Suffixed'.
--
-- >>> "preview" ^? prefixed "pre"
-- Just "view"
--
-- >>> suffixed ".o" # "hello"
-- "hello.o"
--
-- Finally, it's possible to traverse, fold over, and map over
-- index-value pairs thanks to instances of
-- 'Control.Lens.Indexed.TraversableWithIndex',
-- 'Control.Lens.Indexed.FoldableWithIndex', and
-- 'Control.Lens.Indexed.FunctorWithIndex'.
--
-- >>> imap (,) "Hello"
-- [(0,'H'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
-- >>> ifoldMap replicate "Hello"
-- "ellllloooo"
--
-- >>> itraverse_ (curry print) "Hello"
-- (0,'H')
-- (1,'e')
-- (2,'l')
-- (3,'l')
-- (4,'o')
--
----------------------------------------------------------------------------
module Data.List.Lens
  ( stripSuffix
  ) where

import Prelude ()

import Control.Monad (guard)
import Control.Lens.Internal.Prelude

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

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
