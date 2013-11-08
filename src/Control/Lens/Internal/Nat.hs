{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Nat
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-------------------------------------------------------------------------------
module Control.Lens.Internal.Nat
       ( Z, S
       , Add, Subtract
       , N0, N1, N2, N3, N4, N5, N6, N7, N8
       , False, True
       , GT
       ) where

data Z
data S a

data False
data True

type family Add x y
type instance Add Z y = y
type instance Add (S x) y = S (Add x y)

type family Subtract x y
type instance Subtract Z x = x
type instance Subtract (S x) (S y) = Subtract x y

type family GT x y
type instance GT Z x = False
type instance GT (S x) Z = True
type instance GT (S x) (S y) = GT x y

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
