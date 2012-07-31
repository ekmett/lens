-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Morphism
  ( Morphism(..)
  , Isomorphism(..)
  , from
  ) where

import Control.Category
import Prelude hiding ((.),id)

----------------------------------------------------------------------------
-- Isomorphism Implementation Details
-----------------------------------------------------------------------------

-- | Used to provide overloading of isomorphism application
class Category k => Morphism k where
  -- | Build this morphism out of an isomorphism
  --
  -- The intention is that by using 'morphism', you can supply both halves of an
  -- isomorphism, but k can be isntantiated to (->), so you can freely use
  -- the resulting isomorphism as a function.
  morphism :: (a -> b) -> (b -> a) -> k a b

  -- | Map a morphism using an isomorphism between morphisms
  isomap :: ((a -> b) -> c -> d) -> ((b -> a) -> d -> c) -> k a b -> k c d

instance Morphism (->) where
  morphism = const
  isomap = const

-- | Exposed because under some circumstances you may need to manually employ hither and yon.
data Isomorphism a b = Isomorphism { hither :: a -> b, yon :: b -> a }

instance Category Isomorphism where
  id = Isomorphism id id
  Isomorphism bc cb . Isomorphism ab ba = Isomorphism (bc . ab) (ba . cb)

instance Morphism Isomorphism where
  morphism = Isomorphism
  isomap abcd badc (Isomorphism ab ba) = Isomorphism (abcd ab) (badc ba)

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
from :: Morphism k => Isomorphism a b -> k b a
from (Isomorphism a b) = morphism b a
{-# SPECIALIZE from :: Isomorphism a b -> b -> a #-}
{-# SPECIALIZE from :: Isomorphism a b -> Isomorphism b a #-}

