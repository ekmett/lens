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
data Leftmost a
  = LPure                 -- ^ No element.
  | LLeaf a               -- ^ One element.
  | LStep (Leftmost a)    -- ^ The result of '<>'.
  | LFound (Leftmost a) a -- ^ The elements of the first field, then an element.
  | LSkip (Leftmost a)    -- ^ The same elements as the field.

-- The mirror image of the 'Semigroup' instance of 'Rightmost' below. See the
-- comment there. Here '<>' looks inside an 'LSkip' in @x@. So @x '<>' y@
-- does not force @y@ when @x@ is an 'LLeaf' or an 'LFound', with or without
-- 'LSkip' around it.
instance Semigroup (Leftmost a) where
  x <> y = LStep (appendLeftmost x y)

-- | The field of the 'LStep' of @x '<>' y@. When @x@ is 'LPure', it returns
-- @'LSkip' y@, not @y@, so that it never ends by evaluating @y@.
appendLeftmost :: Leftmost a -> Leftmost a -> Leftmost a
appendLeftmost x y = case x of
  LSkip x'   -> appendLeftmost x' y
  LPure      -> LSkip y
  LLeaf _    -> x
  LFound _ _ -> x
  LStep x'   -> appendLeftmostStep x' y

-- | 'appendLeftmost' when @x@ is @'LStep' x'@.
appendLeftmostStep :: Leftmost a -> Leftmost a -> Leftmost a
appendLeftmostStep x' y = case y of
  LSkip y'    -> appendLeftmostStep x' y'
  LPure       -> LSkip x'
  -- These two cases give Just as soon as there is an element,
  -- so firstOf (backwards folded) [1..] is Just _|_.
  LLeaf a     -> LFound x' a
  LFound y' a -> LFound x' (firstFrom y' a)
  LStep y'    -> x' <> y'

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
  {-# INLINE mappend #-}
#endif

-- | The first element of the first argument, or the second argument if it has none.
firstFrom :: Leftmost a -> a -> a
firstFrom LPure        a = a
firstFrom (LLeaf b)    _ = b
firstFrom (LFound l b) _ = firstFrom l b
firstFrom (LSkip l)    a = firstFrom l a
firstFrom (LStep l)    a = firstFrom l a

-- | Extract the 'Leftmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost LPure = Nothing
getLeftmost (LLeaf a) = Just a
getLeftmost (LFound l a) = Just (firstFrom l a)
getLeftmost (LSkip x) = getLeftmost x
getLeftmost (LStep x) = getLeftmost x

-- | Used for 'Control.Lens.Fold.lastOf'.
data Rightmost a
  = RPure                  -- ^ No element.
  | RLeaf a                -- ^ One element.
  | RStep (Rightmost a)    -- ^ The result of '<>'.
  | RFound a (Rightmost a) -- ^ An element, then the elements of the second field.
  | RSkip (Rightmost a)    -- ^ The same elements as the field.

-- Reading a long result of '<>' must not keep a stack frame for each '<>'.
-- So the field of an 'RStep' never ends by evaluating another field. Where
-- that field would be a value that is not evaluated yet, '<>' puts 'RSkip'
-- around it. 'RFound' holds an element and the values after it in two
-- fields, so 'lastFrom' can read them in a loop. '<>' looks inside an 'RSkip'
-- in @y@. So @x '<>' y@ does not force @x@ when @y@ is an 'RLeaf' or an
-- 'RFound', with or without 'RSkip' around it.
instance Semigroup (Rightmost a) where
  x <> y = RStep (appendRightmost x y)

-- | The field of the 'RStep' of @x '<>' y@. When @y@ is 'RPure', it returns
-- @'RSkip' x@, not @x@, so that it never ends by evaluating @x@.
appendRightmost :: Rightmost a -> Rightmost a -> Rightmost a
appendRightmost x y = case y of
  RSkip y'   -> appendRightmost x y'
  RPure      -> RSkip x
  RLeaf _    -> y
  RFound _ _ -> y
  RStep y'   -> appendRightmostStep x y'

-- | 'appendRightmost' when @y@ is @'RStep' y'@.
appendRightmostStep :: Rightmost a -> Rightmost a -> Rightmost a
appendRightmostStep x y' = case x of
  RSkip x'    -> appendRightmostStep x' y'
  RPure       -> RSkip y'
  -- These two cases give Just as soon as there is an element,
  -- so lastOf folded [1..] is Just _|_.
  RLeaf a     -> RFound a y'
  RFound a x' -> RFound (lastFrom a x') y'
  RStep x'    -> x' <> y'

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
  {-# INLINE mappend #-}
#endif

-- | The last element of the second argument, or the first argument if it has none.
lastFrom :: a -> Rightmost a -> a
lastFrom a RPure        = a
lastFrom _ (RLeaf b)    = b
lastFrom _ (RFound b r) = lastFrom b r
lastFrom a (RSkip r)    = lastFrom a r
lastFrom a (RStep r)    = lastFrom a r

-- | Extract the 'Rightmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getRightmost :: Rightmost a -> Maybe a
getRightmost RPure = Nothing
getRightmost (RLeaf a) = Just a
getRightmost (RFound a r) = Just (lastFrom a r)
getRightmost (RSkip x) = getRightmost x
getRightmost (RStep x) = getRightmost x
