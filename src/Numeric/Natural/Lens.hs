{-# language RankNTypes #-}
{-# language PatternGuards #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Natural.Lens
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Useful tools for Gödel numbering.
-------------------------------------------------------------------------------
module Numeric.Natural.Lens
  ( _Pair
  , _Sum
  , _Naturals
  , pattern Pair
  , pattern Sum
  , pattern Naturals
  ) where

import Control.Lens
import Numeric.Natural

-- | The natural numbers are isomorphic to the product of the natural numbers with itself.
--
-- @N = N*N@
_Pair :: Iso' Natural (Natural, Natural)
_Pair = iso hither (uncurry yon) where
  yon 0 0 = 0
  yon m n = case quotRem m 2 of
    (q,r) -> r + 2 * yon n q -- rotation

  hither 0 = (0,0)
  hither n = case quotRem n 2 of
   (p,r) -> case hither p of
     (x,y) -> (r+2*y,x) -- rotation

-- | The natural numbers are isomorphic to disjoint sums of natural numbers embedded as
-- evens or odds.
--
-- @N = 2*N@
_Sum :: Iso' Natural (Either Natural Natural)
_Sum = iso hither yon where
  hither p = case quotRem p 2 of
    (q,0) -> Left q
    (q,1) -> Right q
    _     -> error "_Sum: impossible"
  yon (Left q)  = 2*q
  yon (Right q) = 2*q+1

-- | The natural numbers are isomorphic to lists of natural numbers
_Naturals :: Iso' Natural [Natural]
_Naturals = iso hither yon where
  hither 0 = []
  hither n | (h, t) <- (n-1)^._Pair = h : hither t
  yon [] = 0
  yon (x:xs) = 1 + review _Pair (x, yon xs)

-- |
-- interleaves the bits of two natural numbers
pattern Pair :: Natural -> Natural -> Natural
pattern Pair x y <- (view _Pair -> (x,y)) where
  Pair x y = review _Pair (x,y)

-- |
-- @
-- Sum (Left q) = 2*q
-- Sum (Right q) = 2*q+1
-- @
pattern Sum :: Either Natural Natural -> Natural
pattern Sum s <- (view _Sum -> s) where
  Sum s = review _Sum s

-- |
-- @
-- Naturals [] = 0
-- Naturals (h:t) = 1 + Pair h (Naturals t)
-- @
pattern Naturals :: [Natural] -> Natural
pattern Naturals xs <- (view _Naturals -> xs) where
  Naturals xs = review _Naturals xs
