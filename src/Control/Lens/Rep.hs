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
-- Consider the following example.
--
-- > import Control.Lens
-- > import Control.Lens.Rep
-- > import Data.Distributive
--
-- > data Pair a = Pair { _x :: a, _y :: a }
--
-- > makeLenses ''Pair
--
-- > instance Representable Pair where
-- >   rep f = Pair (f x) (f y)
--
-- From there, you can get definitions for a number of instances for free.
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
  -- * Wrapped Representations
  , Key(..)
  , keys
  -- * Traversal with representation
  , mapWithRep
  , foldMapWithRep
  , foldrWithRep
  , traverseWithRep
  , traverseWithRep_
  , forWithRep
  , mapMWithRep
  , mapMWithRep_
  , forMWithRep
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
-- A 'Functor' @f@ is 'Representable' if it is isomorphic to @(x -> a)@
-- for some x. All such functors can be represented by choosing @x@ to be
-- the set of lenses that are polymorphic in the contents of the 'Functor',
-- that is to say @x = Rep f@ is a valid choice of 'x' for every 
-- 'Representable' 'Functor'.
--
-- Note: Some sources refer to covariant representable functors as
-- corepresentable functors, and leave the \"representable\" name to
-- contravariant functors (those are isomorphic to @(a -> x)@ for some @x@).
--
-- As the covariant case is vastly more common, and both are often referred to
-- as representable functors, we choose to call these functors 'Representable'
-- here.

class Functor f => Representable f where
  rep :: (Rep f -> a) -> f a

instance Representable Identity where
  rep f = Identity (f identityL)

instance Eq e => Representable ((->) e) where
  rep f e = f (funL e)

-- | 'fmapRep' is a valid default definition for 'fmap' for a representable
-- functor.
--
-- > fmapRep f m = rep $ \i -> f (m^.i)
--
-- Usage for a representable functor @Foo@:
--
-- > instance Functor Foo where
-- >   fmap = fmapRep

fmapRep :: Representable f => (a -> b) -> f a -> f b
fmapRep f m = rep $ \i -> f (m^.i)
{-# INLINE fmapRep #-}

-- | 'pureRep' is a valid default definition for 'pure' and 'return' for a
-- representable functor.
--
-- > pureRep = rep . const
--
-- Usage for a representable functor @Foo@:
--
-- > instance Applicative Foo where
-- >    pure = pureRep
-- >    (<*>) = apRep
--
-- > instance Monad Foo where
-- >   return = pureRep
-- >   (>>=) = bindRep
pureRep :: Representable f => a -> f a
pureRep = rep . const
{-# INLINE pureRep #-}

-- | 'apRep' is a valid default definition for '(<*>)' for a representable
-- functor.
--
-- > apRep mf ma = rep $ \i -> mf^.i $ ma^.i
--
-- Usage for a representable functor @Foo@:
--
-- > instance Applicative Foo where
-- >    pure = pureRep
-- >   (<*>) = apRep
apRep :: Representable f => f (a -> b) -> f a -> f b
apRep mf ma = rep $ \i -> mf^.i $ ma^.i
{-# INLINE apRep #-}

-- | 'bindRep' is a valid default default definition for '(>>=)' for a
-- representable functor.
--
-- > bindRep m f = rep $ \i -> f(m^.i)^.i
--
-- Usage for a representable functor @Foo@:
--
-- > instance Monad ... where
-- >   return = pureRep
-- >   (>>=) = bindRep
bindRep :: Representable f => f a -> (a -> f b) -> f b
bindRep m f = rep $ \i -> f(m^.i)^.i
{-# INLINE bindRep #-}

-- | A default definition for 'Data.Distributive.distribute' for a 'Representable' 'Functor'
--
-- > distributeRep wf = rep $ \i -> fmap (^.i) wf
--
-- Typical Usage:
--
-- > instance Distributive ... where
-- >   distribute = distributeRep
distributeRep :: (Representable f, Functor w) => w (f a) -> f (w a)
distributeRep wf = rep $ \i -> fmap (^.i) wf
{-# INLINE distributeRep #-}

-----------------------------------------------------------------------------
-- Keys
-----------------------------------------------------------------------------

-- | Sometimes you need to store a path lens into a container, but at least
-- at this time, impredicative polymorphism in GHC is somewhat lacking.
--
-- This type provides a way to, say, store a list of polymorphic lenses.
newtype Key f = Key { turn :: Rep f }

-- | A 'Representable' 'Functor' has a fixed shape. This fills each position 
-- in it with a 'Key'
keys :: Representable f => f (Key f)
keys = rep Key
{-# INLINE keys #-}

-----------------------------------------------------------------------------
-- Traversal
-----------------------------------------------------------------------------


-- | Map over a 'Representable' 'Functor' with access to the lens for the 
-- current position
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

-- | Traverse a 'Representable' 'Functor' with access to the current path
-- as a lens, discarding the result
traverseWithRep_ :: (Representable f, Foldable f, Applicative g)
                 => (Rep f -> a -> g b) -> f a -> g ()
traverseWithRep_ f m = sequenceA_ (mapWithRep f m)
{-# INLINE traverseWithRep_ #-}

-- | Traverse a 'Representable' 'Functor' with access to the current path
-- and a lens (and the arguments flipped)
forWithRep :: (Representable f, Traversable f, Applicative g)
                => f a -> (Rep f -> a -> g b) -> g (f b)
forWithRep m f = sequenceA (mapWithRep f m)
{-# INLINE forWithRep #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path
-- as a lens
mapMWithRep :: (Representable f, Traversable f, Monad m)
                => (Rep f -> a -> m b) -> f a -> m (f b)
mapMWithRep f m = Traversable.sequence (mapWithRep f m)
{-# INLINE mapMWithRep #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path
-- as a lens, discarding the result
mapMWithRep_ :: (Representable f, Foldable f, Monad m)
                 => (Rep f -> a -> m b) -> f a -> m ()
mapMWithRep_ f m = Foldable.sequence_ (mapWithRep f m)
{-# INLINE mapMWithRep_ #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path
-- as a lens (with the arguments flipped)
forMWithRep :: (Representable f, Traversable f, Monad m)
                => f a -> (Rep f -> a -> m b) -> m (f b)
forMWithRep m f = Traversable.sequence (mapWithRep f m)
{-# INLINE forMWithRep #-}

-- | Fold over a 'Representable' 'Functor' with access to the current path
-- as a lens, yielding a 'Monoid'
foldMapWithRep :: (Representable f, Foldable f, Monoid m)
               => (Rep f -> a -> m) -> f a -> m
foldMapWithRep f m = fold (mapWithRep f m)
{-# INLINE foldMapWithRep #-}

-- | Fold over a 'Representable' 'Functor' with access to the current path
-- as a lens.
foldrWithRep :: (Representable f, Foldable f) => (Rep f -> a -> b -> b) -> b -> f a -> b
foldrWithRep f b m = Foldable.foldr id b (mapWithRep f m)
{-# INLINE foldrWithRep #-}

