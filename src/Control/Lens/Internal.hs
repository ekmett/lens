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
    Gettable(..)
  , Accessor(..)
  , Settable(..)
  , Mutator(..)
  -- * Implementation details
  , IndexedStore(..)
  , Focusing(..)
  , Traversed(..)
  , Sequenced(..)
  , AppliedState(..)
  , Min(..)
  , getMin
  , Max(..)
  , getMax
  , ElementOf(..)
  , ElementOfResult(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Category
import Prelude hiding ((.),id)
import Data.Monoid

-----------------------------------------------------------------------------
-- Gettables & Accessors
-----------------------------------------------------------------------------
-- this requires a phantom type
class Functor f => Gettable f where
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose

newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m

instance Gettable (Accessor r) where
  coerce (Accessor m) = Accessor m

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  Accessor a <*> Accessor b = Accessor (mappend a b)

-----------------------------------------------------------------------------
-- Settables & Mutators
-----------------------------------------------------------------------------

class Applicative f => Settable f where
  run :: f a -> a

instance Settable Identity where
  run = runIdentity

instance Settable f => Settable (Backwards f) where
  run = run . forwards

instance (Settable f, Settable g) => Settable (Compose f g) where
  run = run . run . getCompose

newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator (f a)

instance Applicative Mutator where
  pure = Mutator
  Mutator f <*> Mutator a = Mutator (f a)

instance Settable Mutator where
  run = runMutator

-----------------------------------------------------------------------------
-- Functors
-----------------------------------------------------------------------------

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

-- | Used internally by 'mapM_' and the like.
newtype Sequenced m = Sequenced { getSequenced :: m () }

instance Monad m => Monoid (Sequenced m) where
  mempty = Sequenced (return ())
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)

-- | Used for 'minimumOf'
data Min a = NoMin | Min a

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  mappend NoMin m = m
  mappend m NoMin = m
  mappend (Min a) (Min b) = Min (min a b)

-- | Obtain the minimum
getMin :: Min a -> Maybe a
getMin NoMin   = Nothing
getMin (Min a) = Just a

-- | Used for 'maximumOf'
data Max a = NoMax | Max a

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  mappend NoMax m = m
  mappend m NoMax = m
  mappend (Max a) (Max b) = Max (max a b)

-- | Obtain the maximum
getMax :: Max a -> Maybe a
getMax NoMax   = Nothing
getMax (Max a) = Just a

-- | The result of trying to find the nth element of a 'Traversal'.
data ElementOfResult f a
  = Searching {-# UNPACK #-} !Int a
  | Found {-# UNPACK #-} !Int (f a)

instance Functor f => Functor (ElementOfResult f) where
  fmap f (Searching i a) = Searching i (f a)
  fmap f (Found i as) = Found i (fmap f as)

-- | Used to find the nth element of a 'Traversal'.
data ElementOf f a = ElementOf { getElementOf :: Int -> ElementOfResult f a }

instance Functor f => Functor (ElementOf f) where
  fmap f (ElementOf m) = ElementOf $ \i -> case m i of
    Searching j a -> Searching j (f a)
    Found j as -> Found j (fmap f as)

instance Functor f => Applicative (ElementOf f) where
  pure a = ElementOf $ \i -> Searching i a
  ElementOf mf <*> ElementOf ma = ElementOf $ \i -> case mf i of
    Found j ff -> case ma j of
      Found _ _ -> error "elementOf: found multiple results"
      Searching k a -> Found k (fmap ($a) ff)
    Searching j f -> case ma j of
      Found k as -> Found k (fmap f as)
      Searching k a -> Searching k (f a)

