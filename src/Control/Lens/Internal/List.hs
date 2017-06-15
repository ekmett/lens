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
  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

#ifdef HLINT
{-# ANN module "HLint: ignore Redundant bracket" #-}
#endif

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
