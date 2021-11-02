{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Fold
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Fold
  (
  -- * Monoids for folding
    Folding(..)
  , Traversed(..)
  , TraversedF(..)
  , Sequenced(..)
  , Leftmost(..), getLeftmost
  , Rightmost(..), getRightmost
  , ReifiedMonoid(..)
  -- * Semigroups for folding
  , NonEmptyDList(..)
  ) where

import Prelude ()

import Control.Lens.Internal.Getter
import Control.Lens.Internal.Prelude
import Data.Functor.Bind
import Data.Maybe (fromMaybe)
import Data.Reflection

import qualified Data.List.NonEmpty as NonEmpty

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | A 'Monoid' for a 'Contravariant' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Contravariant f, Applicative f) => Semigroup (Folding f a) where
  Folding fr <> Folding fs = Folding (fr *> fs)
  {-# INLINE (<>) #-}

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Traversed: value used"))
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- TraversedF
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Fold.traverse1Of_' and the like.
--
-- @since 4.16
newtype TraversedF a f = TraversedF { getTraversedF :: f a }

instance Apply f => Semigroup (TraversedF a f) where
  TraversedF ma <> TraversedF mb = TraversedF (ma .> mb)
  {-# INLINE (<>) #-}

instance (Apply f, Applicative f) => Monoid (TraversedF a f) where
  mempty = TraversedF (pure (error "TraversedF: value used"))
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  TraversedF ma `mappend` TraversedF mb = TraversedF (ma *> mb)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
--
-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Monad m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Sequenced a m) where
  mempty = Sequenced (return (error "Sequenced: value used"))
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- NonEmptyDList
------------------------------------------------------------------------------

newtype NonEmptyDList a
  = NonEmptyDList { getNonEmptyDList :: [a] -> NonEmpty.NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  NonEmptyDList f <> NonEmptyDList g = NonEmptyDList (f . NonEmpty.toList . g)

------------------------------------------------------------------------------
-- Leftmost and Rightmost
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.firstOf'.
data Leftmost a = LPure | LLeaf a | LStep (Leftmost a)

instance Semigroup (Leftmost a) where
  x <> y = LStep $ case x of
    LPure    -> y
    LLeaf _  -> x
    LStep x' -> case y of
      -- The last two cases make firstOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> x <> y'.
      -- Note that this means that firstOf (backwards folded) [1..] is Just _|_.
      LPure    -> x'
      LLeaf a  -> LLeaf $ fromMaybe a (getLeftmost x')
      LStep y' -> mappend x' y'

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
  {-# INLINE mappend #-}
#endif

-- | Extract the 'Leftmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost LPure = Nothing
getLeftmost (LLeaf a) = Just a
getLeftmost (LStep x) = getLeftmost x

-- | Used for 'Control.Lens.Fold.lastOf'.
data Rightmost a = RPure | RLeaf a | RStep (Rightmost a)

instance Semigroup (Rightmost a) where
  x <> y = RStep $ case y of
    RPure    -> x
    RLeaf _  -> y
    RStep y' -> case x of
      -- The last two cases make lastOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> x <> y'.
      -- Note that this means that lastOf folded [1..] is Just _|_.
      RPure    -> y'
      RLeaf a  -> RLeaf $ fromMaybe a (getRightmost y')
      RStep x' -> mappend x' y'

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
  {-# INLINE mappend #-}
#endif

-- | Extract the 'Rightmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getRightmost :: Rightmost a -> Maybe a
getRightmost RPure = Nothing
getRightmost (RLeaf a) = Just a
getRightmost (RStep x) = getRightmost x
