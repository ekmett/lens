{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Equality
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Control.Lens.Equality
  (
  -- * Type Equality
    Equality, Equality'
  , AnEquality, AnEquality'
  , runEq
  , substEq
  , mapEq
  , fromEq
  -- * Implementation Details
  , Identical(..)
  ) where

import Control.Lens.Internal
import Control.Lens.Type

{-# ANN module "HLint: ignore Use id" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}

-- $setup
-- >>> import Control.Lens

-----------------------------------------------------------------------------
-- Equality
-----------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects an 'Equality'.
type AnEquality s t a b = Identical a (Mutator b) a (Mutator b) -> Identical a (Mutator b) s (Mutator t)

-- | A 'Simple' 'AnEquality'
type AnEquality' s a = AnEquality s s a a

-- | Extract a witness of type equality
runEq :: AnEquality s t a b -> Identical s t a b
runEq l = case l Identical of Identical -> Identical

-- | Substituting types with equality
substEq :: AnEquality s t a b -> ((s ~ a, t ~ b) => r) -> r
substEq l = case runEq l of
  Identical -> \r -> r

-- | We can use equality to do substitution into anything
mapEq :: AnEquality s t a b -> f s -> f a
mapEq l r = substEq l r

-- | Equality is symmetric
fromEq :: AnEquality s t a b -> Equality b a t s
fromEq l = substEq l id
