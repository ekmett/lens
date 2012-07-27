{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- These are some of the explicit Functor instances that leak into the
-- type signatures of Control.Lens. You shouldn't need to import this
-- module directly, unless you are coming up with a whole new kind of
-- \"Family\" and need to add instances.
--
----------------------------------------------------------------------------
module Control.Lens.Internal
  (
  -- * Implementation details
    IndexedStore(..)
  , Focusing(..)
  , Traversed(..)
  , AppliedState(..)
  ) where

import Control.Applicative
import Data.Monoid

-- | Used by 'Focus'

newtype Focusing m c a = Focusing { unfocusing :: m (c, a) }

instance Monad m => Functor (Focusing m c) where
  fmap f (Focusing m) = Focusing $ do
     (c, a) <- m
     return (c, f a)

instance (Monad m, Monoid c) => Applicative (Focusing m c) where
  pure a = Focusing (return (mempty, a))
  Focusing mf <*> Focusing ma = Focusing $ do
    (c, f) <- mf
    (d, a) <- ma
    return (mappend c d, f a)

-- | The indexed store can be used to characterize a 'LensFamily'
-- and is used by 'clone'

data IndexedStore c d a = IndexedStore (d -> a) c

instance Functor (IndexedStore c d) where
  fmap f (IndexedStore g c) = IndexedStore (f . g) c

-- | Applicative composition of @State Int@ with a 'Functor', used
-- by 'elementOf', 'elementsOf', 'traverseElement', 'traverseElementsOf'

newtype AppliedState f a = AppliedState { runAppliedState :: Int -> (f a, Int) }

instance Functor f => Functor (AppliedState f) where
  fmap f (AppliedState m) = AppliedState $ \i -> case m i of
    (fa, j) -> (fmap f fa, j)

instance Applicative f => Applicative (AppliedState f) where
  pure a = AppliedState (\i -> (pure a, i))
  AppliedState mf <*> AppliedState ma = AppliedState $ \i -> case mf i of
    (ff, j) -> case ma j of
       (fa, k) -> (ff <*> fa, k)

-- | Used internally by 'traverseOf_', 'mapM_' and the like.

newtype Traversed f = Traversed { getTraversed :: f () }

instance Applicative f => Monoid (Traversed f) where
  mempty = Traversed (pure ())
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
