{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Isomorphic
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types
--
----------------------------------------------------------------------------
module Control.Isomorphic
  ( Isomorphic(..)
  , Isomorphism(..)
  , from
  , via
  , (:~>)
  ) where

import Control.Category
import Prelude hiding ((.),id)
import Data.Typeable

----------------------------------------------------------------------------
-- Isomorphism Implementation Details
-----------------------------------------------------------------------------

-- | An isomorphism from a to b, overloaded to permit its use directly as a function.
--
-- You can use a value of type @(a :~ b)@ as if it were @(a -> b)@ or @Isomorphism a b@.
infixr 0 :~>
type a :~> b = forall k. Isomorphic k => k a b

-- | Used to provide overloading of isomorphism application
--
-- This is a 'Category' with a canonical mapping to it from the
-- category of isomorphisms over Haskell types.
class Category k => Isomorphic k where
  -- | Build this morphism out of an isomorphism
  --
  -- The intention is that by using 'isomorphic', you can supply both halves of an
  -- isomorphism, but k can be instantiated to (->), so you can freely use
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
  deriving (Typeable)

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
-- If you imported 'Control.Category.(.)', then:
--
-- > from l . from r = from (r . l)
--
-- > from :: (a :~> b) -> (b :~> a)
from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a
{-# INLINE from #-}
{-# SPECIALIZE from :: Isomorphism a b -> b -> a #-}
{-# SPECIALIZE from :: Isomorphism a b -> Isomorphism b a #-}

-- |
-- > via :: Isomorphism a b -> (a :~> b)
via :: Isomorphic k => Isomorphism a b -> k a b
via (Isomorphism a b) = isomorphic a b
{-# INLINE via #-}
{-# SPECIALIZE via :: Isomorphism a b -> a -> b #-}
{-# SPECIALIZE via :: Isomorphism a b -> Isomorphism a b #-}
