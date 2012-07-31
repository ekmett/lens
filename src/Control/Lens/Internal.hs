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
  , Action(..)
  , AppliedState(..)
  , Min(..)
  , getMin
  , Max(..)
  , getMax
  , Isomorphism(..)
  , Isomorphic(..)
  ) where

import Control.Applicative
import Control.Category
import Prelude hiding ((.),id)
import Data.Monoid

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
newtype Action m = Action { getAction :: m () }

instance Monad m => Monoid (Action m) where
  mempty = Action (return ())
  Action ma `mappend` Action mb = Action (ma >> mb)

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

-----------------------------------------------------------------------------
-- Isomorphism Implementation Details
-----------------------------------------------------------------------------

-- | Used by the iso smart constructor to overload function application.
class Category k => Isomorphic k where
  -- | Build this morphism out of an isomorphism
  morphism :: (a -> b) -> (b -> a) -> k a b

instance Isomorphic (->) where
  morphism = const

-- | Exposed because under some circumstances you may need to manually employ hither and yon.
data Isomorphism a b = Isomorphism { hither :: a -> b, yon :: b -> a }

instance Category Isomorphism where
  id = Isomorphism id id
  Isomorphism bc cb . Isomorphism ab ba = Isomorphism (bc . ab) (ba . cb)

instance Isomorphic Isomorphism where
  morphism = Isomorphism
