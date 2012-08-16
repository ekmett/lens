{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Isomorphic
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Control.Lens.Isomorphic
  ( Isomorphic(..)
  , Isomorphism(..)
  , from
  , via
  ) where

import Control.Category
import Data.Typeable
import Prelude hiding ((.),id)

----------------------------------------------------------------------------
-- Isomorphism Implementation Details
-----------------------------------------------------------------------------

-- | Used to provide overloading of isomorphism application
--
-- This is a 'Category' with a canonical mapping to it from the
-- category of isomorphisms over Haskell types.
class Category k => Isomorphic k where
  -- | Build this morphism out of an isomorphism
  --
  -- The intention is that by using 'isomorphic', you can supply both halves of an
  -- isomorphism, but k can be instantiated to @(->)@, so you can freely use
  -- the resulting isomorphism as a function.
  isomorphic :: (a -> b) -> (b -> a) -> k a b

  -- | Map a morphism in the target category using an isomorphism between morphisms
  -- in Hask.
  isomap :: ((a -> b) -> c -> d) -> ((b -> a) -> d -> c) -> k a b -> k c d

instance Isomorphic (->) where
  isomorphic = const
  {-# INLINE isomorphic #-}
  isomap = const
  {-# INLINE isomap #-}

-- | A concrete data type for isomorphisms.
--
-- This lets you place an isomorphism inside a container without using @ImpredicativeTypes@.
data Isomorphism a b = Isomorphism (a -> b) (b -> a)
  deriving Typeable

instance Category Isomorphism where
  id = Isomorphism id id
  {-# INLINE id #-}
  Isomorphism bc cb . Isomorphism ab ba = Isomorphism (bc . ab) (ba . cb)
  {-# INLINE (.) #-}

instance Isomorphic Isomorphism where
  isomorphic = Isomorphism
  {-# INLINE isomorphic #-}
  isomap abcd badc (Isomorphism ab ba) = Isomorphism (abcd ab) (badc ba)
  {-# INLINE isomap #-}

-- | Invert an isomorphism.
--
-- Note to compose an isomorphism and receive an isomorphism in turn you'll need to use
-- 'Control.Category.Category'
--
-- > from (from l) = l
--
-- If you imported 'Control.Category..' from @Control.Category@, then:
--
-- > from l . from r = from (r . l)
from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a
{-# INLINE from #-}
{-# SPECIALIZE from :: Isomorphism a b -> b -> a #-}
{-# SPECIALIZE from :: Isomorphism a b -> Isomorphism b a #-}

-- | Convert from an 'Isomorphism' back to any 'Isomorphic' value.
--
-- This is useful when you need to store an isomoprhism as a data type inside a container
-- and later reconstitute it as an overloaded function.
via :: Isomorphic k => Isomorphism a b -> k a b
via (Isomorphism a b) = isomorphic a b
{-# INLINE via #-}
{-# SPECIALIZE via :: Isomorphism a b -> a -> b #-}
{-# SPECIALIZE via :: Isomorphism a b -> Isomorphism a b #-}
