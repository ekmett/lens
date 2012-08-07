{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Indexed
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.Indexed
  (
  -- * Indexed Functions
    Indexed(..)
  , Indexable
  , Index(..)
  , (.@)
  , icompose
  , reindex

  -- * Indexed Setter
  , IndexedSetter
  , imapOf
  , (%@)

  -- * Indexed Traversals
  , IndexedTraversal
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Indexed Folds
  , IndexedFold
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf
  , ianyOf
  , iallOf
  , itraverseOf_
  , iforOf_
  , imapMOf_
  , iforMOf_
  , iconcatMapOf
  -- , imaximumByOf , iminimumByOf , ifindOf
  , ifoldrOf'
  , ifoldlOf'
  , ifoldrMOf
  , ifoldlMOf

  -- * Simple
  , SimpleIndexedTraversal
  , SimpleIndexedSetter
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Lens.Type
import Control.Monad
import Control.Monad.State.Class as State
import Control.Monad.Trans.State.Lazy as Lazy
import Data.Monoid

-- | Permit overloading of function application for things that also admit a notion of a key or index.

-- | Provides overloading for indexed functions.
class Indexed i k where
  -- | Build a function from an indexed function
  index :: ((i -> a) -> b) -> k a b

-- | Type alias for passing around polymorphic indexed functions.
type Indexable i a b = forall k. Indexed i k => k a b

instance Indexed i (->) where
  index f = f . const
  {-# INLINE index #-}

-- | A function with access to a index. This constructor may be useful when you need to store
-- a 'HasIndex'.
newtype Index i a b = Index { withIndex :: (i -> a) -> b }

-- | Using an equality witness to avoid potential overlapping instances
-- and aid dispatch.
instance i ~ j => Indexed i (Index j) where
  index = Index
  {-# INLINE index #-}

-- | Remap the index.
reindex :: Indexed j k => (i -> j) -> Index i a b -> k a b
reindex ij (Index iab) = index $ \ ja -> iab $ \i -> ja (ij i)
{-# SPECIALIZE reindex :: (i -> j) -> Index i a b -> Index j a b #-}
{-# SPECIALIZE reindex :: (i -> j) -> Index i a b -> a -> b #-}

infixr 9 .@
-- | Composition of indexed functions
(.@) :: Indexed (i, j) k => Index i b c -> Index j a b -> k a c
f .@ g = icompose (,) f g
{-# INLINE (.@) #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> Index (i,j) a c #-}
{-# SPECIALIZE (.@) :: Index i b c -> Index j a b -> a -> c #-}

-- | Composition of indexed functions with a user supplied function for combining indexs
icompose :: Indexed k r => (i -> j -> k) -> Index i b c -> Index j a b -> r a c
icompose ijk (Index ibc) (Index jab) = index $ \ka -> ibc $ \i -> jab $ \j -> ka (ijk i j)
{-# INLINE icompose #-}
{-# SPECIALIZE icompose :: (i -> j -> k) -> Index i b c -> Index j a b -> a -> c #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Every 'IndexedFold' is a valid 'Fold'.
type IndexedFold i a c = forall k f b d. (Indexed i k, Applicative f, Gettable f) => k (c -> f d) (a -> f b)

type IndexedFolding i m a b c d = Index i (c -> Accessor m d) (a -> Accessor m b)

-- |
--
-- > ifoldMapOf :: Monoid m => IndexedFold i a c          -> (i -> c -> m) -> a -> m
-- > ifoldMapOf :: Monoid m => IndexedTraversal i a b c d -> (i -> c -> m) -> a -> m
ifoldMapOf :: IndexedFolding i m a b c d -> (i -> c -> m) -> a -> m
ifoldMapOf l f = runAccessor . withIndex l (\i -> Accessor . f i)
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- > ifoldrOf :: IndexedFold i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf :: IndexedTraversal i a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf :: IndexedFolding i (Endo e) a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf l f z t = appEndo (ifoldMapOf l (\i -> Endo . f i) t) z
{-# INLINE ifoldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- > foldl = foldlOf folded
--
-- > ifoldlOf :: IndexedFold i a c          -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf :: IndexedTraversal i a b c d -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf :: IndexedFolding i (Dual (Endo e)) a b c d -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf l f z t = appEndo (getDual (ifoldMapOf l (\i -> Dual . Endo . flip (f i)) t)) z
{-# INLINE ifoldlOf #-}


-- |
-- > ianyOf :: IndexedFold i a c          -> (i -> c -> Bool) -> a -> Bool
-- > ianyOf :: IndexedTraversal i a b c d -> (i -> c -> Bool) -> a -> Bool
ianyOf :: IndexedFolding i Any a b c d -> (i -> c -> Bool) -> a -> Bool
ianyOf l f = getAny . ifoldMapOf l (\i -> Any . f i)
{-# INLINE ianyOf #-}

-- |
-- > iallOf :: IndexedFold i a c          -> (i -> c -> Bool) -> a -> Bool
-- > iallOf :: IndexedTraversal i a b c d -> (i -> c -> Bool) -> a -> Bool
iallOf :: IndexedFolding i All a b c d -> (i -> c -> Bool) -> a -> Bool
iallOf l f = getAll . ifoldMapOf l (\i -> All . f i)
{-# INLINE iallOf #-}

-- |
-- > itraverseOf_ :: Applicative f => IndexedFold i a c          -> (i -> c -> f e) -> a -> f ()
-- > itraverseOf_ :: Applicative f => IndexedTraversal i a b c d -> (i -> c -> f e) -> a -> f ()
itraverseOf_ :: Functor f => IndexedFolding i (Traversed f) a b c d -> (i -> c -> f e) -> a -> f ()
itraverseOf_ l f = getTraversed . ifoldMapOf l (\i -> Traversed . void . f i)
{-# INLINE itraverseOf_ #-}

-- |
-- > iforOf_ :: Applicative f => IndexedFold i a c          -> a -> (i -> c -> f e) -> f ()
-- > iforOf_ :: Applicative f => IndexedTraversal i a b c d -> a -> (i -> c -> f e) -> f ()
iforOf_ :: Functor f => IndexedFolding i (Traversed f) a b c d -> a -> (i -> c -> f e) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- |
-- > imapMOf_ :: Monad m => IndexedFold i a c          -> (i -> c -> m e) -> a -> m ()
-- > imapMOf_ :: Monad m => IndexedTraversal i a b c d -> (i -> c -> m e) -> a -> m ()
imapMOf_ :: Monad m => IndexedFolding i (Sequenced m) a b c d -> (i -> c -> m e) -> a -> m ()
imapMOf_ l f = getSequenced . ifoldMapOf l (\i -> Sequenced . liftM skip . f i)
{-# INLINE imapMOf_ #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

-- |
-- > iforMOf_ :: Monad m => IndexedFold i a c          -> a -> (i -> c -> m e) -> m ()
-- > iforMOf_ :: Monad m => IndexedTraversal i a b c d -> a -> (i -> c -> m e) -> m ()
iforMOf_ :: Monad m => IndexedFolding i (Sequenced m) a b c d -> a -> (i -> c -> m e) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- |
-- > iconcatMapOf :: IndexedFold i a c          -> (i -> c -> [e]) -> a -> [e]
-- > iconcatMapOf :: IndexedTraversal i a b c d -> (i -> c -> [e]) -> a -> [e]
iconcatMapOf :: IndexedFolding i [e] a b c d -> (i -> c -> [e]) -> a -> [e]
iconcatMapOf l ices = runAccessor . withIndex l (\i -> Accessor . ices i)
{-# INLINE iconcatMapOf #-}

{-
-- |
-- Obtain the maximum element (if any) targeted by an 'IndexedFold' or 'IndexedTraversal'
-- according to a user supplied ordering with access to the indices, returning the index and result of the winning entry
--
-- > imaximumByOf :: IndexedFold a c          -> (i -> i -> c -> c -> Ordering) -> a -> Maybe (i, c)
-- > imaximumByOf :: IndexedTraversal a b c d -> (i -> i -> c -> c -> Ordering) -> a -> Maybe (i, c)
imaximumByOf :: IndexedFolding i (Endo (Maybe c)) a b c d -> (i -> i -> c -> c -> Ordering) -> a -> Maybe (i, c)
imaximumByOf l cmp = ifoldrOf l step Nothing where
  step i a Nothing  = Just (i, a)
  step i a (Just (j, b)) = Just $! if cmp i j a b == GT then (i, a) else (j, b)
{-# INLINE imaximumByOf #-}

-- |
-- Obtain the minimum element (if any) targeted by an 'IndexedFold' or 'IndexedTraversal'
-- according to a user supplied ordering with access to the indices, returning the index and result of the winning entry
--
-- > iminimumByOf :: IndexedFold a c          -> (i -> i -> c -> c -> Ordering) -> a -> Maybe (i, c)
-- > iminimumByOf :: IndexedTraversal a b c d -> (i -> i -> c -> c -> Ordering) -> a -> Maybe (i, c)
iminimumByOf :: IndexedFolding i (Endo (Maybe c)) a b c d -> (i -> i -> c -> c -> Ordering) -> a -> Maybe (i, c)
iminimumByOf l cmp = ifoldrOf l step Nothing where
  step i a Nothing  = Just (i, a)
  step i a (Just (j, b)) = Just $! if cmp i j a b == GT then (j, b) else (i, a)
{-# INLINE iminimumByOf #-}

-- | The 'findOf' function takes an IndexedFold or IndexedTraversal, a predicate,
-- a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- > ifindOf :: IndexedFold a c          -> (i -> c -> Bool) -> a -> Maybe (i, c)
-- > ifindOf :: IndexedTraversal a b c d -> (i -> c -> Bool) -> a -> Maybe (i, c)
ifindOf :: IndexedFolding i (First c) a b c d -> (i -> c -> Bool) -> a -> Maybe (i, c)
ifindOf l p = getFirst . ifoldMapOf l step where
  step i c
    | p i c     = First (Just (i, c))
    | otherwise = First Nothing
{-# INLINE ifindOf #-}
-}

-- | Strictly fold right over the elements of a structure with an index.
--
-- > ifoldrOf' :: IndexedFold i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf' :: IndexedTraversal i a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf' :: IndexedFolding i (Dual (Endo (e -> e))) a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but strictly.
--
-- > ifoldlOf' :: IndexedFold i a c            -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf' :: IndexedTraversal i a b c d   -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf' :: IndexedFolding i (Endo (e -> e)) a b c d -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- > ifoldrMOf :: Monad m => IndexedFold i a c          -> (i -> c -> e -> m e) -> e -> a -> e
-- > ifoldrMOf :: Monad m => IndexedTraversal i a b c d -> (i -> c -> e -> m e) -> e -> a -> e
ifoldrMOf :: Monad m => IndexedFolding i (Dual (Endo (e -> m e))) a b c d -> (i -> c -> e -> m e) -> e -> a -> m e
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- > ifoldlOf' :: Monad m => IndexedFold i a c            -> (i -> e -> c -> m e) -> e -> a -> e
-- > ifoldlOf' :: Monad m => IndexedTraversal i a b c d   -> (i -> e -> c -> m e) -> e -> a -> e
ifoldlMOf :: Monad m => IndexedFolding i (Endo (e -> m e)) a b c d -> (i -> e -> c -> m e) -> e -> a -> m e
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Every indexed traversal is a valid Traversal or indexed fold.
--
-- The Traversal laws are still required to hold.
type IndexedTraversal i a b c d = forall f k. (Indexed i k, Applicative f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i a b = IndexedTraversal i a a b b

-- | Traversal with an index.
--
-- > itraverseOf = withIndex
--
-- > itraverseOf :: IndexedTraversal i a b c d -> (i -> c -> f d) -> a -> f b
itraverseOf :: Overloaded (Index i) f a b c d -> (i -> c -> f d) -> a -> f b
itraverseOf = withIndex
{-# INLINE itraverseOf #-}

-- |
-- > iforOf = flip . itraverseOf
iforOf :: Overloaded (Index i) f a b c d -> a -> (i -> c -> f d) -> f b
iforOf = flip . withIndex
{-# INLINE iforOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- > imapMOf :: Monad m => IndexedTraversal a b c d -> (i -> c -> m d) -> a -> m b
imapMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> (i -> c -> m d) -> a -> m b
imapMOf l f = unwrapMonad . withIndex l (\i -> WrapMonad . f i)
{-# INLINE imapMOf #-}

-- |
-- > iforMOf = flip . imapMOf
iforMOf :: Overloaded (Index i) (WrappedMonad m) a b c d -> a -> (i -> c -> m d) -> m b
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal'.
--
-- 'imapAccumROf' accumulates state from right to left.
--
imapAccumROf :: Overloaded (Index i) (Lazy.State s) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumROf l f s0 a = swap (Lazy.runState (withIndex l (\i c -> State.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalized 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal'.
--
-- 'imapAccumLOf' accumulates state from left to right.
imapAccumLOf :: Overloaded (Index i) (Backwards (Lazy.State s)) a b c d -> (i -> s -> c -> (s, d)) -> s -> a -> (s, b)
imapAccumLOf l f s0 a = swap (Lazy.runState (forwards (withIndex l (\i c -> Backwards (State.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

-- | Every 'IndexedSetter' is a valid 'Setter'
--
-- The 'Setter' laws are still required to hold.
type IndexedSetter i a b c d = forall f k. (Indexed i k, Settable f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIdexedTraversal i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedSetter i a b = IndexedSetter i a a b b

-- | Map with index
--
-- > imapOf :: IndexedTraversal i a b c d -> (i -> c -> d) -> a -> b
-- > imapOf :: IndexedSetter i a b c d -> (i -> c -> d) -> a -> b
imapOf :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
imapOf l f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE imapOf #-}

infixr 4 %@

-- | > (%@) = imapOf
(%@) :: Overloaded (Index i) Mutator a b c d -> (i -> c -> d) -> a -> b
l %@ f = runMutator . withIndex l (\i -> Mutator . f i)
{-# INLINE (%@) #-}
