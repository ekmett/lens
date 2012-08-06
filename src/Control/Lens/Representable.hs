{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Representable
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  RankNTypes
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
-- > import Control.Lens.Representable
-- > import Control.Lens.TH
-- > import Control.Applicative
-- > import Data.Foldable
-- > import Data.Traversable
-- > import Data.Distributive
--
-- > data Pair a = Pair { _x :: a, _y :: a }
--
-- > makeLenses ''Pair
--
-- > instance Representable Pair where
-- >   rep f = Pair <$> f x <*> f y
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
-- > instance Foldable Pair where
-- >   foldMap = foldMapRep
--
-- > instance Traversable Pair where
-- >   traverse = traverseRep
--
-- > instance Distributive Pair where
-- >   distribute = distributeRep
--
----------------------------------------------------------------------------
module Control.Lens.Representable
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
  , foldMapRep
  , traverseRep
  , distributeRep
  -- * Wrapped Representations
  , Key(..)
  , keys
  , tabulated
  -- * Traversal with representation
  , mapWithRep
  , foldMapWithRep
  , foldrWithRep
  , traverseWithRep
  , forWithRep
  , mapMWithRep
  , forMWithRep
  ) where

import Control.Applicative
import Control.Isomorphic
import Control.Lens
import Data.Functor.Identity
import Data.Monoid

-- | The representation of a 'Representable' 'Functor' as Lenses
type Rep f = forall a. Simple Lens (f a) a

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
  repA :: Applicative g => (Rep f -> g a) -> g (f a)
  
  rep :: (Rep f -> a) -> f a
  rep f = runIdentity $ repA (Identity . f)


instance Representable Identity where
  repA f = Identity <$> f (from identity)

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

-- | A default definition for 'Data.Foldable.foldMap' for a 'Representable' 'Functor'
--
-- Typical Usage:
--
-- > instance Foldable ... where
-- >   foldMap = foldMapRep
foldMapRep :: (Representable f, Monoid m) => (a -> m) -> f a -> m
foldMapRep f = getConst . traverseRep (Const . f)
{-# INLINE foldMapRep #-}

-- | A default definition for 'Data.Traversable.traverse' for a 'Representable' 'Functor'
--
-- > traverseRep f m = repA $ \i -> f (m^.i)
--
-- Typical Usage:
--
-- > instance Traversable ... where
-- >   traverse = traverseRep
traverseRep :: (Representable f, Applicative g) => (a -> g b) -> f a -> g (f b)
traverseRep f m = repA $ \i -> f (m^.i)
{-# INLINE traverseRep #-}

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

-- | A version of 'rep' that is an isomorphism. Predicativity requires that
-- we wrap the 'Rep' as a 'Key', however.
tabulated :: Representable f => (Key f -> a) :~> f a
tabulated = isomorphic (\f -> rep (f . Key)) (\fa key -> view (turn key) fa)
{-# INLINE tabulated #-}

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
traverseWithRep :: (Representable f, Applicative g)
                => (Rep f -> a -> g b) -> f a -> g (f b)
traverseWithRep f m = repA $ \i -> f i (m^.i)
{-# INLINE traverseWithRep #-}

-- | Traverse a 'Representable' 'Functor' with access to the current path
-- and a lens (and the arguments flipped)
forWithRep :: (Representable f, Applicative g)
                => f a -> (Rep f -> a -> g b) -> g (f b)
forWithRep = flip traverseWithRep
{-# INLINE forWithRep #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path
-- as a lens
mapMWithRep :: (Representable f, Monad m)
                => (Rep f -> a -> m b) -> f a -> m (f b)
mapMWithRep f = unwrapMonad . traverseWithRep (\i -> WrapMonad . f i)
{-# INLINE mapMWithRep #-}

-- | 'mapM' over a 'Representable' 'Functor' with access to the current path
-- as a lens (with the arguments flipped)
forMWithRep :: (Representable f, Monad m)
                => f a -> (Rep f -> a -> m b) -> m (f b)
forMWithRep = flip mapMWithRep
{-# INLINE forMWithRep #-}

-- | Fold over a 'Representable' 'Functor' with access to the current path
-- as a lens, yielding a 'Monoid'
foldMapWithRep :: (Representable f, Monoid m)
               => (Rep f -> a -> m) -> f a -> m
foldMapWithRep f = getConst . traverseWithRep (\i -> Const . f i)
{-# INLINE foldMapWithRep #-}

-- | Fold over a 'Representable' 'Functor' with access to the current path
-- as a lens.
foldrWithRep :: Representable f => (Rep f -> a -> b -> b) -> b -> f a -> b
foldrWithRep f z t = appEndo (foldMapWithRep (\i -> Endo . f i) t) z
{-# INLINE foldrWithRep #-}

