{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedFold
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.IndexedFold
  (
  -- * Indexed Folds
    IndexedFold

  -- * Consuming Indexed Folds
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
  , ifindOf
  , ifoldrOf'
  , ifoldlOf'
  , ifoldrMOf
  , ifoldlMOf
  , itoListOf

  -- * Converting to Folds
  , withIndicesOf
  , indicesOf

  -- * Building Indexed Folds
  , ifiltering
  , ibackwards
  , itakingWhile
  , idroppingWhile

  -- * Storing Indexed Folds
  , ReifiedIndexedFold(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Indexed
import Control.Lens.IndexedGetter
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad
import Data.Monoid

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Every 'IndexedFold' is a valid 'Control.Lens.Fold.Fold'.
type IndexedFold i s a = forall k f. (Indexed i k, Applicative f, Gettable f) => k (a -> f a) (s -> f s)

-- |
-- Fold an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldMapOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldMapOf' l ≡ 'ifoldMapOf' l '.' 'const'@
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i a s          -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i a s            -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' ::             'Control.Lens.IndexedLens.SimpleIndexedLens' i a s      -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a s -> (i -> s -> m) -> a -> m
-- @
ifoldMapOf :: IndexedGetting i m s t a b -> (i -> a -> m) -> s -> m
ifoldMapOf l f = runAccessor . withIndex l (\i -> Accessor . f i)
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf' l ≡ 'ifoldrOf' l '.' 'const'@
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i s a          -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedFold' i s a            -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf :: IndexedGetting i (Endo r) s t a b -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l f z t = appEndo (ifoldMapOf l (\i -> Endo . f i) t) z
{-# INLINE ifoldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf' l ≡ 'ifoldlOf' l '.' 'const'@
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i s a          -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedFold' i s a            -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo r)) s t a b -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z t = appEndo (getDual (ifoldMapOf l (\i -> Dual . Endo . flip (f i)) t)) z
{-# INLINE ifoldlOf #-}

-- |
-- Return whether or not any element viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.anyOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.anyOf' l ≡ 'ianyOf' l '.' 'const'@
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i s a          -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i s a            -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any s t a b -> (i -> a -> Bool) -> s -> Bool
ianyOf l f = getAny . ifoldMapOf l (\i -> Any . f i)
{-# INLINE ianyOf #-}

-- |
-- Return whether or not all elements viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.allOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.allOf' l ≡ 'iallOf' l '.' 'const'@
--
-- @
-- 'iallOf' :: 'IndexedGetter' i s a          -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i s a            -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
iallOf :: IndexedGetting i All s t a b -> (i -> a -> Bool) -> s -> Bool
iallOf l f = getAll . ifoldMapOf l (\i -> All . f i)
{-# INLINE iallOf #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index @i@, discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.traverseOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.traverseOf_' l ≡ 'itraverseOf' l '.' 'const'@
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i s a          -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i s a            -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> f r) -> s -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed f) s t a b -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l f = getTraversed . ifoldMapOf l (\i -> Traversed . void . f i)
{-# INLINE itraverseOf_ #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @'iforOf_' ≡ 'flip' '.' 'itraverseOf_'@
--
-- When you don't need access to the index then 'Control.Lens.Fold.forOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forOf_' l a ≡ 'iforOf_' l a '.' 'const'@
--
-- @
-- 'iforOf_' :: 'Functor' f     => 'IndexedGetter' i s a          -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedFold' i s a            -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Functor' f     => 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> s -> (i -> a -> f r) -> f ()
-- @
iforOf_ :: Functor f => IndexedGetting i (Traversed f) s t a b -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.mapMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.mapMOf_' l ≡ 'imapMOf' l '.' 'const'@
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i s a          -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i s a            -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> m r) -> s -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced m) s t a b -> (i -> a -> m r) -> s -> m ()
imapMOf_ l f = getSequenced . ifoldMapOf l (\i -> Sequenced . liftM skip . f i)
{-# INLINE imapMOf_ #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @'iforMOf_' ≡ 'flip' '.' 'imapMOf_'@
--
-- When you don't need access to the index then 'Control.Lens.Fold.forMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forMOf_' l a ≡ 'iforMOf' l a '.' 'const'@
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i s a          -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i s a            -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> s -> (i -> a -> m r) -> m ()
-- @
iforMOf_ :: Monad m => IndexedGetting i (Sequenced m) s t a b -> s -> (i -> a -> m r) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- |
-- Concatenate the results of a function of the elements of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- When you don't need access to the index then 'Control.Lens.Fold.concatMapOf'  is more flexible in what it accepts.
--
-- @
-- 'Control.Lens.Fold.concatMapOf' l ≡ 'iconcatMapOf' l '.' 'const'
-- 'iconcatMapOf' ≡ 'ifoldMapOf'
-- @
--
-- @
-- 'iconcatMapOf' :: 'IndexedGetter' i s a          -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedFold' i s a            -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> [r]) -> s -> [r]
-- @
iconcatMapOf :: IndexedGetting i [r] s t a b -> (i -> a -> [r]) -> s -> [r]
iconcatMapOf = ifoldMapOf
{-# INLINE iconcatMapOf #-}

-- | The 'findOf' function takes an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'Control.Lens.Fold.findOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.findOf' l ≡ 'ifindOf' l '.' 'const'@
--
-- @
-- 'ifindOf' :: 'IndexedGetter' s a          -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'IndexedFold' s a            -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' s a      -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' s a -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- @
ifindOf :: IndexedGetting i (First (i, a)) s t a b -> (i -> a -> Bool) -> s -> Maybe (i, a)
ifindOf l p = getFirst . ifoldMapOf l step where
  step i a
    | p i a     = First $ Just (i, a)
    | otherwise = First Nothing
{-# INLINE ifindOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrOf'' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf'' l ≡ 'ifoldrOf'' l '.' 'const'@
--
-- @
-- 'ifoldrOf'' :: 'IndexedGetter' i s a          -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedFold' i s a            -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf' :: IndexedGetting i (Dual (Endo (r -> r))) s t a b -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlOf'' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'@
--
-- @
-- 'ifoldlOf'' :: 'IndexedGetter' i s a            -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedFold' i s a              -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a        -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a   -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf' :: IndexedGetting i (Endo (r -> r)) s t a b -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrMOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrMOf' l ≡ 'ifoldrMOf' l '.' 'const'@
--
-- @
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedGetter' i s a          -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedFold' i s a            -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> (i -> a -> r -> m r) -> r -> s -> r
-- @
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (r -> m r))) s t a b -> (i -> a -> r -> m r) -> r -> s -> m r
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlMOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlMOf' l ≡ 'ifoldlMOf' l '.' 'const'@
--
-- @
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedGetter' i s a            -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedFold' i s a              -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a        -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a   -> (i -> r -> a -> m r) -> r -> s -> r
-- @
ifoldlMOf :: Monad m => IndexedGetting i (Endo (r -> m r)) s t a b -> (i -> r -> a -> m r) -> r -> s -> m r
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'Control.Lens.Fold.toListOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.toListOf' l ≡ 'map' 'fst' '.' 'itoListOf' l@
--
-- @
-- 'itoListOf' :: 'IndexedGetter' i s a          -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedFold' i s a            -> s -> [(i,a)]
-- 'itoListOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i s a      -> s -> [(i,a)]
-- 'itoListOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i s a -> s -> [(i,a)]
-- @
itoListOf :: IndexedGetting i [(i,a)] s t a b -> s -> [(i,a)]
itoListOf l = ifoldMapOf l (\i a -> [(i,a)])
{-# INLINE itoListOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Transform an indexed fold into a fold of both the indices and the values.
--
-- @
-- 'withIndicesOf' :: 'IndexedFold' i s a             -> 'Fold' s (i, a)
-- 'withIndicesOf' :: 'SimpleIndexedLens' i s a      -> 'Getter' s (i, a)
-- 'withIndicesOf' :: 'SimpleIndexedTraversal' i s a -> 'Fold' s (i, a)
-- @
--
-- All 'Fold' operations are safe, and comply with the laws. However,
--
-- Passing this an 'IndexedTraversal' will still allow many
-- 'Traversal' combinators to type check on the result, but the result
-- can only be legally traversed by operations that do not edit the indices.
--
-- @
-- 'withIndicesOf' :: 'IndexedTraversal' i s t a b -> 'Traversal' s t (i, a) (j, b)
-- @
--
-- Change made to the indices will be discarded.
withIndicesOf :: Functor f => Overloaded (Index i) f s t a b -> LensLike f s t (i, a) (j, b)
withIndicesOf l f = withIndex l (\i c -> snd <$> f (i,c))
{-# INLINE withIndicesOf #-}

-- | Transform an indexed fold into a fold of the indices.
--
-- @
-- 'indicesOf' :: 'IndexedFold' i s a             -> 'Fold' s i
-- 'indicesOf' :: 'SimpleIndexedLens' i s a      -> 'Getter' s i
-- 'indicesOf' :: 'SimpleIndexedTraversal' i s a -> 'Fold' s i
-- @
indicesOf :: Gettable f => Overloaded (Index i) f s t a a -> LensLike f s t i j
indicesOf l f = withIndex l (const . coerce . f)
{-# INLINE indicesOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Obtain an 'IndexedFold' by filtering an 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter', or 'IndexedFold'.
--
-- When passed an 'Control.Lens.IndexedTraversal.IndexedTraversal', sadly the result is /not/ a legal 'Control.Lens.IndexedTraversal.IndexedTraversal'.
--
-- See 'filtered' for a related counter-example.
ifiltering :: (Applicative f, Indexed i k) => (i -> a -> Bool) -> Index i (a -> f a) (s -> f t) -> k (a -> f a) (s -> f t)
ifiltering p l = index $ \ f -> withIndex l $ \ i c -> if p i c then f i c else pure c
{-# INLINE ifiltering #-}

-- | Reverse the order of the elements of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'.
-- This has no effect on an 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter', or 'Control.Lens.IndexedSetter.IndexedSetter'.
ibackwards :: Indexed i k => Index i (a -> (Backwards f) b) (s -> (Backwards f) t) -> k (a -> f b) (s -> f t)
ibackwards l = index $ \ f -> fmap forwards . withIndex l $ \ i -> Backwards . f i
{-# INLINE ibackwards #-}

-- | Obtain an 'IndexedFold' by taking elements from another 'IndexedFold', 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter' or 'Control.Lens.IndexedTraversal.IndexedTraversal' while a predicate holds.
itakingWhile :: (Gettable f, Applicative f, Indexed i k)
            => (i -> a -> Bool)
            -> IndexedGetting i (Endo (f s)) s s a a
            -> k (a -> f a) (s -> f s)
itakingWhile p l = index $ \ f -> ifoldrOf l (\i a r -> if p i a then f i a *> r else noEffect) noEffect
{-# INLINE itakingWhile #-}


-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter' or 'Control.Lens.IndexedTraversal.IndexedTraversal' while a predicate holds.
idroppingWhile :: (Gettable f, Applicative f, Indexed i k)
              => (i -> a -> Bool)
              -> IndexedGetting i (Endo (f s, f s)) s s a a
              -> k (a -> f a) (s -> f s)
idroppingWhile p l = index $ \f -> fst . ifoldrOf l (\i a r -> let s = f i a *> snd r in if p i a then (fst r, s) else (s, s)) (noEffect, noEffect)
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Reifying Indexed Folds
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedFold i s a = ReifyIndexedFold { reflectIndexedFold :: IndexedFold i s a }
