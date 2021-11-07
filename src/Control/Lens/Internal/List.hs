{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.List
-- Copyright   :  (C) 2014-2016 Edward Kmett and Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides utility functions on lists used by the library
-- implementation.
-------------------------------------------------------------------------------
module Control.Lens.Internal.List
  ( ordinalNub
  , stripSuffix
  ) where

import Control.Monad (guard)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

--- $setup
--- >>> :set -XNoOverloadedStrings
--- >>> import Control.Lens.Internal.List

-- | Return the the subset of given ordinals within a given bound
-- and in order of the first occurrence seen.
--
-- Bound: @0 <= x < l@
--
-- >>> ordinalNub 3 [-1,2,1,4,2,3]
-- [2,1]
ordinalNub ::
  Int   {- ^ strict upper bound -} ->
  [Int] {- ^ ordinals           -} ->
  [Int] {- ^ unique, in-bound ordinals, in order seen -}
ordinalNub l xs = foldr (ordinalNubHelper l) (const []) xs IntSet.empty

ordinalNubHelper :: Int -> Int -> (IntSet -> [Int]) -> (IntSet -> [Int])
ordinalNubHelper l x next seen
  | outOfBounds || notUnique = next seen
  | otherwise                = x : next (IntSet.insert x seen)
  where
  outOfBounds = x < 0 || l <= x
  notUnique   = x `IntSet.member` seen

-- | \(\mathcal{O}(\min(m,n))\). The 'stripSuffix' function drops the given
-- suffix from a list. It returns 'Nothing' if the list did not end with the
-- suffix given, or 'Just' the list after the suffix, if it does.
--
-- >>> stripSuffix "bar" "foobar"
-- Just "foo"
--
-- >>> stripSuffix "foo" "foo"
-- Just ""
--
-- >>> stripSuffix "bar" "barfoo"
-- Nothing
--
-- >>> stripSuffix "foo" "barfoobaz"
-- Nothing
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
