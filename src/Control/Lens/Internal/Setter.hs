{-# LANGUAGE CPP #-}

{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Setter
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Setter
  (
  -- ** Setters
    Settable(..)
  ) where

import Prelude ()

import Control.Applicative.Backwards
import Control.Lens.Internal.Prelude
import Data.Distributive

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class (Applicative f, Distributive f, Traversable f) => Settable f where
  untainted :: f a -> a

  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot g = g `seq` rmap untainted g
  {-# INLINE untaintedDot #-}

  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot g = g `seq` rmap pure g
  {-# INLINE taintedDot #-}

-- | So you can pass our 'Control.Lens.Setter.Setter' into combinators from other lens libraries.
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = (runIdentity #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Identity #.)
  {-# INLINE taintedDot #-}

-- CAUTION: While Data.Tuple.Solo may *look* a lot like Identity, and while we
-- *could* give it a Settable instance, we probably do not want to do so. In
-- particular, if we did, then Control.Lens.Traversal.over' would "work" with
-- Setters. But ... it wouldn't *actually* work; the mapping would end up being
-- lazy when it's supposed to be strict. Similarly, the BoxT applicative
-- transformer must not be made Settable, because that would cause a similarly
-- confusing problem with Control.Lens.Traversal.strictly. There is not, as
-- yet, any compelling reason to write such an instance, so let's not.

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untaintedDot forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untaintedDot (untaintedDot getCompose)
  {-# INLINE untainted #-}

