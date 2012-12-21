{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , (^@..)

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
  , itakingWhile
  , idroppingWhile

  -- * Storing Indexed Folds
  , ReifiedIndexedFold(..)

  -- * Deprecated
  , ibackwards
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Classes
import Control.Lens.Indexed
import Control.Lens.IndexedGetter
import Control.Lens.Internal
import Control.Lens.Internal.Composition
import Control.Lens.Type
import Control.Monad
import Data.Monoid
import Data.Profunctor

infixr 8 ^@..

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Every 'IndexedFold' is a valid 'Control.Lens.Fold.Fold'.
type IndexedFold i s a = forall k f.
  (Indexable i k, Applicative f, Gettable f) => k a (f a) -> s -> f s

-- |
-- Fold an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldMapOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldMapOf' l ≡ 'ifoldMapOf' l '.' 'const'@
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i a s          -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i a s            -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' ::             'Control.Lens.IndexedLens.IndexedLens'' i a s      -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i a s -> (i -> s -> m) -> a -> m
-- @
ifoldMapOf :: IndexedGetting i m s t a b -> (i -> a -> m) -> s -> m
ifoldMapOf l f = runAccessor #. withIndex l (\i -> Accessor #. f i)
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf' l ≡ 'ifoldrOf' l '.' 'const'@
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i s a          -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedFold' i s a            -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf :: IndexedGetting i (Endo r) s t a b -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l f z t = appEndo (ifoldMapOf l (\i -> Endo #. f i) t) z
{-# INLINE ifoldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf' l ≡ 'ifoldlOf' l '.' 'const'@
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i s a          -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedFold' i s a            -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo r)) s t a b -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z t = appEndo (getDual (ifoldMapOf l (\i -> Dual #. Endo #. flip (f i)) t)) z
{-# INLINE ifoldlOf #-}

-- |
-- Return whether or not any element viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.anyOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.anyOf' l ≡ 'ianyOf' l '.' 'const'@
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i s a          -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i s a            -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any s t a b -> (i -> a -> Bool) -> s -> Bool
ianyOf l f = getAny #. ifoldMapOf l (\i -> Any #. f i)
{-# INLINE ianyOf #-}

-- |
-- Return whether or not all elements viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.allOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.allOf' l ≡ 'iallOf' l '.' 'const'@
--
-- @
-- 'iallOf' :: 'IndexedGetter' i s a          -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i s a            -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
iallOf :: IndexedGetting i All s t a b -> (i -> a -> Bool) -> s -> Bool
iallOf l f = getAll #. ifoldMapOf l (\i -> All #. f i)
{-# INLINE iallOf #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the @i@, discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.traverseOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.traverseOf_' l ≡ 'Control.Lens.IndexedTraversal.itraverseOf' l '.' 'const'@
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i s a          -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i s a            -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> f r) -> s -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed f) s t a b -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l f = getTraversed #. ifoldMapOf l (\i -> Traversed #. void . f i)
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
-- 'iforOf_' :: 'Functor' f     => 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> s -> (i -> a -> f r) -> f ()
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
-- @'Control.Lens.Fold.mapMOf_' l ≡ 'Control.Lens.IndexedSetter.imapMOf' l '.' 'const'@
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i s a          -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i s a            -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> m r) -> s -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced m) s t a b -> (i -> a -> m r) -> s -> m ()
imapMOf_ l f = getSequenced #. ifoldMapOf l (\i -> Sequenced #. liftM skip . f i)
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
-- @'Control.Lens.Fold.forMOf_' l a ≡ 'Control.Lens.IndexedTraversal.iforMOf' l a '.' 'const'@
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i s a          -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i s a            -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> s -> (i -> a -> m r) -> m ()
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
-- 'iconcatMapOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> [r]) -> s -> [r]
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
-- 'ifindOf' :: 'Control.Lens.IndexedLens.IndexedLens'' s a      -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' s a -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- @
ifindOf :: IndexedGetting i (First (i, a)) s t a b -> (i -> a -> Bool) -> s -> Maybe (i, a)
ifindOf l p = getFirst #. ifoldMapOf l step where
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
-- 'ifoldrOf'' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
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
-- 'ifoldlOf'' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a        -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a   -> (i -> r -> a -> r) -> r -> s -> r
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
-- 'ifoldrMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> (i -> a -> r -> m r) -> r -> s -> r
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
-- 'ifoldlOf'' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'' i s a        -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a   -> (i -> r -> a -> m r) -> r -> s -> r
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
-- 'itoListOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> s -> [(i,a)]
-- 'itoListOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> s -> [(i,a)]
-- @
itoListOf :: IndexedGetting i (Endo [(i,a)]) s t a b -> s -> [(i,a)]
itoListOf l = ifoldrOf l (\i a -> ((i,a):)) []
{-# INLINE itoListOf #-}

-- | An infix version of 'itoListOf'

-- @
-- ('^@..') :: s -> 'IndexedGetter' i s a          -> [(i,a)]
-- ('^@..') :: s -> 'IndexedFold' i s a            -> [(i,a)]
-- ('^@..') :: s -> 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> [(i,a)]
-- ('^@..') :: s -> 'Control.Lens.IndexedTraversal.IndexedTraversal'' i s a -> [(i,a)]
-- @
(^@..) :: s -> IndexedGetting i (Endo [(i,a)]) s t a b -> [(i,a)]
s ^@.. l = ifoldrOf l (\i a -> ((i,a):)) [] s

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Transform an indexed fold into a fold of both the indices and the values.
--
-- @
-- 'withIndicesOf' :: 'IndexedFold' i s a            -> 'Control.Lens.Fold.Fold' s (i, a)
-- 'withIndicesOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> 'Control.Lens.Fold.Getter' s (i, a)
-- 'withIndicesOf' :: 'Control.Lens.IndexedLens.IndexedTraversal'' i s a -> 'Control.Lens.Fold.Fold' s (i, a)
-- @
--
-- All 'Control.Lens.Fold.Fold' operations are safe, and comply with the laws. However:
--
-- Passing this an 'Control.Lens.IndexedTraversal.IndexedTraversal' will still allow many
-- 'Control.Lens.Traversal.Traversal' combinators to type check on the result, but the result
-- can only be legally traversed by operations that do not edit the indices.
--
-- @
-- 'withIndicesOf' :: 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> 'Control.Lens.Traversal.Traversal' s t (i, a) (j, b)
-- @
--
-- Change made to the indices will be discarded.
withIndicesOf :: Functor f => IndexedLensLike (Indexed i) f s t a b -> LensLike f s t (i, a) (j, b)
withIndicesOf l f = l @~ \i c -> snd <$> f (i,c)
{-# INLINE withIndicesOf #-}

-- | Transform an indexed fold into a fold of the indices.
--
-- @
-- 'indicesOf' :: 'IndexedFold' i s a            -> 'Control.Lens.Fold.Fold' s i
-- 'indicesOf' :: 'Control.Lens.IndexedLens.IndexedLens'' i s a      -> 'Control.Lens.Fold.Getter' s i
-- 'indicesOf' :: 'Control.Lens.IndexedLens.IndexedTraversal'' i s a -> 'Control.Lens.Fold.Fold' s i
-- @
indicesOf :: Gettable f => IndexedLensLike (Indexed i) f s t a a -> LensLike f s t i j
indicesOf l f = l @~ const . coerce . f
{-# INLINE indicesOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Obtain an 'IndexedFold' by filtering an 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter', or 'IndexedFold'.
--
-- When passed an 'Control.Lens.IndexedTraversal.IndexedTraversal', sadly the result is /not/ a legal 'Control.Lens.IndexedTraversal.IndexedTraversal'.
--
-- See 'Control.Lens.Fold.filtered' for a related counter-example.
ifiltering :: (Applicative f, Indexable i k)
           => (i -> a -> Bool)
           -> (Indexed i a (f a) -> s -> f t)
           -> IndexedLensLike k f s t a a
ifiltering p l f = l @~ \ i c -> if p i c then indexed f i c else pure c
{-# INLINE ifiltering #-}


-- | Obtain an 'IndexedFold' by taking elements from another
-- 'IndexedFold', 'Control.Lens.IndexedLens.IndexedLens',
-- 'IndexedGetter' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- while a predicate holds.
itakingWhile :: (Gettable f, Applicative f, Indexable i k)
             => (i -> a -> Bool)
             -> IndexedGetting i (Endo (f s)) s s a a
             -> IndexedLensLike' k f s a
itakingWhile p l f =
  ifoldrOf l (\i a r -> if p i a then indexed f i a *> r else noEffect) noEffect
{-# INLINE itakingWhile #-}


-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter' or 'Control.Lens.IndexedTraversal.IndexedTraversal' while a predicate holds.
idroppingWhile :: (Gettable f, Applicative f, Indexable i k)
              => (i -> a -> Bool)
              -> IndexedGetting i (Endo (f s, f s)) s s a a
              -> IndexedLensLike' k f s a
idroppingWhile p l f =
  fst . ifoldrOf l
                 (\i a r -> let s = indexed f i a *> snd r in if p i a then (fst r, s) else (s, s))
                 (noEffect, noEffect)
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Reifying Indexed Folds
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedFold i s a =
  ReifyIndexedFold { reflectIndexedFold :: IndexedFold i s a }

ibackwards :: Profunctor k => IndexedLensLike k (Backwards f) s t a b -> IndexedLensLike k f s t a b
ibackwards l f = forwards #. l (rmap Backwards f)
{-# DEPRECATED ibackwards "use backwards" #-}
