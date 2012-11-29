{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Projection
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.IndexedProjection
  (
  -- * Projections
    IndexedProjection
  , IndexedProjectiveLens
  -- * Constructing Projections
  , IndexedProjective(..)
  , IndexedProjecting

{-
  -- * Consuming Projections
  , cloneIndexedProjection
-}

  -- * Simple
  , SimpleIndexedProjection
  , SimpleIndexedProjectiveLens
  ) where

import Control.Applicative
import Control.Lens.Classes
import Control.Lens.Internal
import Control.Lens.Type
import Prelude hiding (id,(.))

------------------------------------------------------------------------------
-- Indexed Projection Internals
------------------------------------------------------------------------------

-- | An 'IndexedProjection' @l@ is a 0-or-1 target 'IndexedTraversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction, such that in addition to the 'IndexedTraversal' laws, we also
-- have the 'Projection' laws.
--
-- Every 'IndexedProjection' is a valid 'IndexedTraversal'.
--
-- You should be able to pass an 'IndexedProjection' to any combinator that actually consumes a 'Projection' as a
-- 'Projecting'. If you need to pass it as a 'Projection', you may have to call 'cloneProjection' on it first.
type IndexedProjection i s t a b = forall k f. (IndexedProjective i k, Applicative f) => k (a -> f b) (s -> f t)

-- | An 'IndexedProjectiveLens' @l@ is an 'IndexedLens' than can also be used as an 'IndexedProjection'
type IndexedProjectiveLens i s t a b = forall k f. (IndexedProjective i k, Functor f) => k (a -> f b) (s -> f t)

-- | A @'Simple' ('IndexedProjection' i)@
type SimpleIndexedProjection i s a = IndexedProjection i s s a a

-- | A @'Simple' ('IndexedProjectiveLens' i)@
type SimpleIndexedProjectiveLens i s a = IndexedProjectiveLens i s s a a

-- | Consume a 'Project'. This is commonly used when a function takes a 'Projection' as a parameter.
type IndexedProjecting i f s t a b = Overloaded (IndexedProject i) f s t a b

{-
-- | Clone a 'Projection' so that you can reuse the same monomorphically typed 'Projection' for different purposes.
--
-- See 'cloneLens' and 'cloneTraversal' for examples of why you might want to do this.
cloneIndexedProjection :: Overloaded (IndexedProject i) (Bazaar a b) s t a b -> IndexedProjection i s t a b
cloneIndexedProjection (IndexedProject f g) = iprojecting (unsafeCoerce f) (cloneIndexedTraversal (unsafeCoerce g))
-}

