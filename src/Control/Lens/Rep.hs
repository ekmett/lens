{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Rep
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Corepresentable endofunctors represented by their polymorphic lenses
--
-- The polymorphic lenses of the form @(forall x. Lens (f x) x)@ each
-- represent a distinct path into a functor @f@. If the functor is entirely
-- characterized by assigning values to these paths, then the functor is
-- representable.
--
-- Consider,
--
-- import Control.Lens
-- import Control.Lens.Rep
-- import Data.Distributive
--
-- > data Pair a = Pair { _x :: a, _y :: a }
--
-- > makeLenses ''Pair
--
-- > instance Representable Pair where
-- >   rep f = Pair (f x) (f y)
--
-- From there, you can get definitions for a number of classes for free.
--
-- > instance Applicative Pair where
-- >   pure  = pureRep
-- >   (<*>) = apRep
--
-- > instance Monad Pair where
-- >   return = pureRep
-- >   (>>=) = bindRep
--
-- > instance Distributive Pair where
-- >   distribute = distributeRep
--
----------------------------------------------------------------------------
module Control.Lens.Rep
  (
  -- * Representable Functors
    Representable(..)
  -- * Using Lenses as Representations
  , Rep
  -- * Default definitions
  , fmapRep
  , pureRep
  , apRep
  , bindRep
  , distributeRep
  , mapWithRep
  , foldMapWithRep
  , foldrWithRep
  , traverseWithRep
  , traverseWithRep_
  , mapMWithRep
  , mapMWithRep_
  -- * Wrapped Representations
  , Key(..)
  , keys
  ) where

import Control.Applicative
import Control.Lens
import Data.Foldable         as Foldable
import Data.Functor.Identity
import Data.Monoid
import Data.Traversable      as Traversable

-- | The representation of a 'Representable' 'Functor' as Lenses
type Rep f = forall a. Lens (f a) a

-- | Representable Functors. 
--
-- A Functor @f@ is representable if it is
-- isomorphic to @(x -> a)@ for some x. All such functors can be represented
-- by choosing @x@ to be the set of lenses that are polymorphic in the contents
-- of the 'Functor', that is to say @x = Rep f@ is a valid choice of 'x' for
-- every 'Representable' 'Functor'.
class Functor f => Representable f where
  rep :: (Rep f -> a) -> f a

instance Representable Identity where
  rep f = Identity (f identityL)

-- | A default definition for 'fmap' for a 'Representable' 'Functor'
--
-- > fmapRep f m = rep $ \i -> f (m^.i)
fmapRep :: Representable f => (a -> b) -> f a -> f b
fmapRep f m = rep $ \i -> f (m^.i)
{-# INLINE fmapRep #-}

-- | A default definition for 'pure' for a 'Representable' 'Functor'
--
-- > pureRep = rep . const
pureRep :: Representable f => a -> f a
pureRep = rep . const
{-# INLINE pureRep #-}

-- | A default definition for '(<*>)' for a 'Representable' 'Functor'
--
-- > apRep mf ma = rep $ \i -> mf^.i $ ma^.i
apRep :: Representable f => f (a -> b) -> f a -> f b
apRep mf ma = rep $ \i -> mf^.i $ ma^.i
{-# INLINE apRep #-}

-- | A default definition for '(>>=)' for a 'Representable' 'Functor'
--
-- > bindRep m f = rep $ \i -> f(m^.i)^.i
bindRep :: Representable f => f a -> (a -> f b) -> f b
bindRep m f = rep $ \i -> f(m^.i)^.i
{-# INLINE bindRep #-}

-- | A default definition for 'distribute' from Data.Distributive for a 'Representable' 'Functor'
--
-- > distributeRep wf = rep $ \i -> fmap (^.i) wf
distributeRep :: (Representable f, Functor w) => w (f a) -> f (w a)
distributeRep wf = rep $ \i -> fmap (^.i) wf
{-# INLINE distributeRep #-}

-- | Map over a 'Representable' 'Functor' with access to the lens for the current position
--
-- > mapWithKey f m = rep $ \i -> f i (m^.i)
mapWithRep :: Representable f => (Rep f -> a -> b) -> f a -> f b
mapWithRep f m = rep $ \i -> f i (m^.i)
{-# INLINE mapWithRep #-}

-- | Traverse a 'Representable' 'Functor' with access to the current path
traverseWithRep :: (Representable f, Traversable f, Applicative g)
                => (Rep f -> a -> g b) -> f a -> g (f b)
traverseWithRep f m = sequenceA (mapWithRep f m)
{-# INLINE traverseWithRep #-}

-- | Traverse a 'Representable' 'Functor' with access to the current path as a lens,
-- discarding the result
traverseWithRep_ :: (Representable f, Foldable f, Applicative g)
                 => (Rep f -> a -> g b) -> f a -> g ()
traverseWithRep_ f m = sequenceA_ (mapWithRep f m)
{-# INLINE traverseWithRep_ #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path
mapMWithRep :: (Representable f, Traversable f, Monad m)
                => (Rep f -> a -> m b) -> f a -> m (f b)
mapMWithRep f m = Traversable.sequence (mapWithRep f m)
{-# INLINE traverseWithRep #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path as a lens,
-- discarding the result
mapMWithRep_ :: (Representable f, Foldable f, Monad m)
                 => (Rep f -> a -> m b) -> f a -> m ()
mapMWithRep_ f m = Foldable.sequence_ (mapWithRep f m)
{-# INLINE traverseWithRep_ #-}

-- | Fold over a a 'Representable' 'Functor' with access to the current path as a lens,
--  yielding a 'Monoid'
foldMapWithRep :: (Representable f, Foldable f, Monoid m)
               => (Rep f -> a -> m) -> f a -> m
foldMapWithRep f m = fold (mapWithRep f m)
{-# INLINE foldMapWithRep #-}

-- | Fold over a a 'Representable' 'Functor' with access to the current path as a lens.
foldrWithRep :: (Representable f, Foldable f) => (Rep f -> a -> b -> b) -> b -> f a -> b
foldrWithRep f b m = Foldable.foldr id b (mapWithRep f m)
{-# INLINE foldrWithRep #-}

-- | Sometimes you need to store a path lens into a container, but at least
-- at this time, impredicative polymorphism in GHC is somewhat lacking.
--
-- This type provides a way to, say, store a list of polymorphic lenses.
newtype Key f = Key { turn :: Rep f }

keys :: Representable f => f (Key f)
keys = rep Key
{-# INLINE keys #-}
