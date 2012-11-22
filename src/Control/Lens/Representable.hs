{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- The polymorphic lenses of the form @(forall x. 'Lens' (f x) x)@ each
-- represent a distinct path into a functor @f@. If the functor is entirely
-- characterized by assigning values to these paths, then the functor is
-- representable.
--
-- Consider the following example.
--
-- > import Control.Lens
-- > import Data.Distributive
--
-- > data Pair a = Pair { _x :: a, _y :: a }
--
-- @ 'Control.Lens.TH.makeLenses' \'\'Pair@
--
-- @
-- instance 'Representable' Pair where
--   'rep' f = Pair (f x) (f y)
-- @
--
-- From there, you can get definitions for a number of instances for free.
--
-- @
-- instance 'Applicative' Pair where
--   'pure'  = 'pureRep'
--   ('<*>') = 'apRep'
-- @
--
-- @
-- instance 'Monad' Pair where
--   'return' = 'pureRep'
--   ('>>=') = 'bindRep'
-- @
--
-- @
-- instance 'Data.Distributive.Distributive' Pair where
--   'Data.Distributive.distribute' = 'distributeRep'
-- @
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
  , distributeRep
  -- * Wrapped Representations
  , Path(..)
  , paths
  , tabulated
  -- * Setting with Representation
  , rmap
  -- * Folding with Representation
  , rfoldMap
  , rfoldr
  -- * Traversing with Representation
  , rtraverse
  , rtraverse_
  , rfor
  , rmapM
  , rmapM_
  , rforM
  -- * Representable Setters, Folds and Traversals
  , rmapped
  , rfolded
  , rtraversed
  ) where

import Control.Applicative
import Control.Lens.Classes
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.IndexedFold
import Control.Lens.IndexedLens
import Control.Lens.IndexedSetter
import Control.Lens.IndexedTraversal
import Control.Lens.Internal
import Control.Lens.Internal.Combinators
import Control.Lens.Iso
import Control.Lens.Type
import Data.Foldable         as Foldable
import Data.Functor.Identity
import Data.Monoid
import Data.Traversable      as Traversable

-- | The representation of a 'Representable' 'Functor' as Lenses
type Rep f = forall a. Simple Lens (f a) a

-- | Representable Functors.
--
-- A 'Functor' @f@ is 'Representable' if it is isomorphic to @(x -> a)@
-- for some x. Nearly all such functors can be represented by choosing @x@ to be
-- the set of lenses that are polymorphic in the contents of the 'Functor',
-- that is to say @x = 'Rep' f@ is a valid choice of 'x' for (nearly) every
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
  rep f = Identity (f (from identity))

-- | NB: The 'Eq' requirement on this instance is a consequence of the choice of 'Lens' as a 'Rep', it isn't fundamental.
instance Eq e => Representable ((->) e) where
  rep f e = f (resultAt e)

-- | 'fmapRep' is a valid default definition for 'fmap' for a 'Representable'
-- functor.
--
-- @'fmapRep' f m = 'rep' '$' \i -> f (m '^.' i)@
--
-- Usage for a @'Representable' Foo@:
--
-- @
-- instance 'Functor' Foo where
--   'fmap' = 'fmapRep'
-- @
fmapRep :: Representable f => (a -> b) -> f a -> f b
fmapRep f m = rep $ \i -> f (m^.i)
{-# INLINE fmapRep #-}

-- | 'pureRep' is a valid default definition for 'pure' and 'return' for a
-- 'Representable' functor.
--
-- @'pureRep' = 'rep' . 'const'@
--
-- Usage for a @'Representable' Foo@:
--
-- @
-- instance 'Applicative' Foo where
--   'pure' = 'pureRep'
--   ...
-- @
--
-- @
-- instance 'Monad' Foo where
--   'return' = 'pureRep'
--   ...
-- @
pureRep :: Representable f => a -> f a
pureRep = rep . const
{-# INLINE pureRep #-}

-- | 'apRep' is a valid default definition for ('<*>') for a 'Representable'
-- functor.
--
-- @'apRep' mf ma = 'rep' '$' \i -> mf '^.' i '$' ma '^.' i@
--
-- Usage for a @'Representable' Foo@:
--
-- @
-- instance 'Applicative' Foo where
--   'pure' = 'pureRep'
--   ('<*>') = 'apRep'
-- @
apRep :: Representable f => f (a -> b) -> f a -> f b
apRep mf ma = rep $ \i -> mf^.i $ ma^.i
{-# INLINE apRep #-}

-- | 'bindRep' is a valid default default definition for '(>>=)' for a
-- representable functor.
--
-- @'bindRep' m f = 'rep' '$' \i -> f (m '^.' i) '^.' i@
--
-- Usage for a @'Representable' Foo@:
--
-- @
-- instance 'Monad' Foo where
--   'return' = 'pureRep'
--   ('>>=') = 'bindRep'
-- @
bindRep :: Representable f => f a -> (a -> f b) -> f b
bindRep m f = rep $ \i -> f(m^.i)^.i
{-# INLINE bindRep #-}

-- | A default definition for 'Data.Distributive.distribute' for a 'Representable' 'Functor'
--
-- @'distributeRep' wf = 'rep' '$' \i -> 'fmap' ('^.' i) wf@
--
-- Usage for a @'Representable' Foo@:
--
-- @
-- instance 'Data.Distributive.Distributive' Foo where
--   'Data.Distributive.distribute' = 'distributeRep'
-- @
distributeRep :: (Representable f, Functor w) => w (f a) -> f (w a)
distributeRep wf = rep $ \i -> fmap (^.i) wf
{-# INLINE distributeRep #-}

-----------------------------------------------------------------------------
-- Paths
-----------------------------------------------------------------------------

-- | Sometimes you need to store a path lens into a container, but at least
-- at this time, @ImpredicativePolymorphism@ in GHC is somewhat lacking.
--
-- This type provides a way to, say, store a @[]@ of paths.
newtype Path f = Path { walk :: Rep f }

-- | A 'Representable' 'Functor' has a fixed shape. This fills each position
-- in it with a 'Path'
paths :: Representable f => f (Path f)
paths = rep Path
{-# INLINE paths #-}

-- | A version of 'rep' that is an isomorphism. Predicativity requires that
-- we wrap the 'Rep' as a 'Key', however.
tabulated :: Representable f => (Path f -> a) -> f a
tabulated = isomorphic (\f -> rep (f . Path)) (\fa path -> view (walk path) fa)
{-# INLINE tabulated #-}

-----------------------------------------------------------------------------
-- Traversal
-----------------------------------------------------------------------------

-- | Map over a 'Representable' functor with access to the 'Lens' for the
-- current position
--
-- @'rmap' f m = 'rep' '$' \i -> f i (m '^.' i)@
rmap :: Representable f => (Rep f -> a -> b) -> f a -> f b
rmap f m = rep $ \i -> f i (m^.i)
{-# INLINE rmap #-}

-- | Traverse a 'Representable' functor with access to the current path
rtraverse :: (Representable f, Traversable f, Applicative g)
          => (Rep f -> a -> g b) -> f a -> g (f b)
rtraverse f m = sequenceA (rmap f m)
{-# INLINE rtraverse #-}

-- | Traverse a 'Representable' functor with access to the current path
-- as a 'Lens', discarding the result
rtraverse_ :: (Representable f, Foldable f, Applicative g)
           => (Rep f -> a -> g b) -> f a -> g ()
rtraverse_ f m = sequenceA_ (rmap f m)
{-# INLINE rtraverse_ #-}

-- | Traverse a 'Representable' functor with access to the current path
-- and a 'Lens' (and the arguments flipped)
rfor :: (Representable f, Traversable f, Applicative g)
     => f a -> (Rep f -> a -> g b) -> g (f b)
rfor m f = sequenceA (rmap f m)
{-# INLINE rfor #-}

-- | 'mapM' over a 'Representable' functor with access to the current path
-- as a 'Lens'
rmapM :: (Representable f, Traversable f, Monad m)
      => (Rep f -> a -> m b) -> f a -> m (f b)
rmapM f m = Traversable.sequence (rmap f m)
{-# INLINE rmapM #-}

-- | 'mapM' over a 'Representable' functor with access to the current path
-- as a 'Lens', discarding the result
rmapM_ :: (Representable f, Foldable f, Monad m)
       => (Rep f -> a -> m b) -> f a -> m ()
rmapM_ f m = Foldable.sequence_ (rmap f m)
{-# INLINE rmapM_ #-}

-- | 'mapM' over a 'Representable' functor with access to the current path
-- as a 'Lens' (with the arguments flipped)
rforM :: (Representable f, Traversable f, Monad m)
      => f a -> (Rep f -> a -> m b) -> m (f b)
rforM m f = Traversable.sequence (rmap f m)
{-# INLINE rforM #-}

-- | Fold over a 'Representable' functor with access to the current path
-- as a 'Lens', yielding a 'Monoid'
rfoldMap :: (Representable f, Foldable f, Monoid m)
         => (Rep f -> a -> m) -> f a -> m
rfoldMap f m = fold (rmap f m)
{-# INLINE rfoldMap #-}

-- | Fold over a 'Representable' functor with access to the current path
-- as a 'Lens'.
rfoldr :: (Representable f, Foldable f) => (Rep f -> a -> b -> b) -> b -> f a -> b
rfoldr f b m = Foldable.foldr id b (rmap f m)
{-# INLINE rfoldr #-}

-- | An 'IndexedSetter' that walks an 'Representable' 'Functor' using a 'Path' for an index.
rmapped :: Representable f => IndexedSetter (Path f) (f a) (f b) a b
rmapped = index $ \f -> tainted# (rmap (\i -> untainted# (f (Path i))))
{-# INLINE rmapped #-}

-- | An 'IndexedFold' that walks an 'Foldable' 'Representable' 'Functor' using a 'Path' for an index.
rfolded :: (Representable f, Foldable f) => IndexedFold (Path f) (f a) a
rfolded = index $ \f -> coerce . getFolding . rfoldMap (\i -> folding# (f (Path i)))
{-# INLINE rfolded #-}

-- | An 'IndexedTraversal' for a 'Traversable' 'Representable' 'Functor'.
rtraversed :: (Representable f, Traversable f) => IndexedTraversal (Path f) (f a) (f b) a b
rtraversed = index $ \ f -> sequenceA . rmap (f . Path)
{-# INLINE rtraversed #-}
