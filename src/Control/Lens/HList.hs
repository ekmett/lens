{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.HList
-- Copyright   :  (C) 2026 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A heterogeneous list, used by 'Control.Lens.TH.makeHListPrisms' in place
-- of tuples. This module is not re-exported by "Control.Lens".
--
-----------------------------------------------------------------------------
module Control.Lens.HList
  ( HList(..)
  ) where

import Data.Kind (Type)

-- | A heterogeneous list, indexed by the list of its element types.
--
-- @
-- 'HNil'                   :: 'HList' '[]
-- 1 ':#' \'a\' ':#' 'HNil'    :: 'HList' '[Int, Char]
-- @
--
-- This is a data family, not a GADT, so a module can match on 'HNil' and
-- @(':#')@ without enabling @GADTs@ or @TypeFamilies@. The price is that a
-- match needs a known index. To recurse over an arbitrary index, use a type
-- class.
data family HList (as :: [Type])

data instance HList '[] = HNil

data instance HList (a ': as) = a :# HList as

infixr 5 :#

instance Show (HList '[]) where
  showsPrec _ HNil = showString "HNil"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  showsPrec d (x :# xs) = showParen (d > 5) $
    showsPrec 6 x . showString " :# " . showsPrec 5 xs

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq a, Eq (HList as)) => Eq (HList (a ': as)) where
  (x :# xs) == (y :# ys) = x == y && xs == ys
