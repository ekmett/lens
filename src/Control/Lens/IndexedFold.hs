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
  , ifiltered
  , itakingWhile
  , idroppingWhile

  -- * Storing Indexed Folds
  , ReifiedIndexedFold(..)
  ) where

import Control.Applicative
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
type IndexedFold i a c = forall k f. (Indexed i k, Applicative f, Gettable f) => k (c -> f c) (a -> f a)

-- |
-- Fold an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldMapOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldMapOf' l = 'ifoldMapOf' l . 'const'@
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i a c          -> (i -> c -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i a c            -> (i -> c -> m) -> a -> m
-- 'ifoldMapOf' ::             'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> m) -> a -> m
-- @
ifoldMapOf :: IndexedGetting i m a c -> (i -> c -> m) -> a -> m
ifoldMapOf l f = runAccessor . withIndex l (\i -> Accessor . f i)
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf' l = 'ifoldrOf' l . 'const'@
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- 'ifoldrOf' :: 'IndexedFold' i a c            -> (i -> c -> e -> e) -> e -> a -> e
-- 'ifoldrOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> e -> e) -> e -> a -> e
-- 'ifoldrOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> e -> e) -> e -> a -> e
-- @
ifoldrOf :: IndexedGetting i (Endo e) a c -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf l f z t = appEndo (ifoldMapOf l (\i -> Endo . f i) t) z
{-# INLINE ifoldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf' l = 'ifoldlOf' l . 'const'@
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i a c          -> (i -> e -> c -> e) -> e -> a -> e
-- 'ifoldlOf' :: 'IndexedFold' i a c            -> (i -> e -> c -> e) -> e -> a -> e
-- 'ifoldlOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> e -> c -> e) -> e -> a -> e
-- 'ifoldlOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> e -> c -> e) -> e -> a -> e
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo e)) a c -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf l f z t = appEndo (getDual (ifoldMapOf l (\i -> Dual . Endo . flip (f i)) t)) z
{-# INLINE ifoldlOf #-}

-- |
-- Return whether or not any element viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.anyOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.anyOf' l = 'ianyOf' l . 'const'@
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i a c          -> (i -> c -> 'Bool') -> a -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i a c            -> (i -> c -> 'Bool') -> a -> 'Bool'
-- 'ianyOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> 'Bool') -> a -> 'Bool'
-- 'ianyOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> 'Bool') -> a -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any a c -> (i -> c -> Bool) -> a -> Bool
ianyOf l f = getAny . ifoldMapOf l (\i -> Any . f i)
{-# INLINE ianyOf #-}

-- |
-- Return whether or not all elements viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'Control.Lens.Fold.allOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.allOf' l = 'iallOf' l . 'const'@
--
-- @
-- 'iallOf' :: 'IndexedGetter' i a c          -> (i -> c -> 'Bool') -> a -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i a c            -> (i -> c -> 'Bool') -> a -> 'Bool'
-- 'iallOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> 'Bool') -> a -> 'Bool'
-- 'iallOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> 'Bool') -> a -> 'Bool'
-- @
iallOf :: IndexedGetting i All a c -> (i -> c -> Bool) -> a -> Bool
iallOf l f = getAll . ifoldMapOf l (\i -> All . f i)
{-# INLINE iallOf #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index @i@, discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.traverseOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.traverseOf_' l = 'itraverseOf' l . 'const'@
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i a c          -> (i -> c -> f e) -> a -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i a c            -> (i -> c -> f e) -> a -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> f e) -> a -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> f e) -> a -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed f) a c -> (i -> c -> f e) -> a -> f ()
itraverseOf_ l f = getTraversed . ifoldMapOf l (\i -> Traversed . void . f i)
{-# INLINE itraverseOf_ #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @'iforOf_' = 'flip' . 'itraverseOf_'@
--
-- When you don't need access to the index then 'Control.Lens.Fold.forOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forOf_' l a = 'iforOf_' l a . 'const'@
--
-- @
-- 'iforOf_' :: 'Functor' f     => 'IndexedGetter' i a c          -> a -> (i -> c -> f e) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedFold' i a c            -> a -> (i -> c -> f e) -> f ()
-- 'iforOf_' :: 'Functor' f     => 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> a -> (i -> c -> f e) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> a -> (i -> c -> f e) -> f ()
-- @
iforOf_ :: Functor f => IndexedGetting i (Traversed f) a c -> a -> (i -> c -> f e) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.mapMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.mapMOf_' l = 'imapMOf' l . 'const'@
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i a c          -> (i -> c -> m e) -> a -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i a c            -> (i -> c -> m e) -> a -> m ()
-- 'imapMOf_' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> m e) -> a -> m ()
-- 'imapMOf_' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> m e) -> a -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced m) a c -> (i -> c -> m e) -> a -> m ()
imapMOf_ l f = getSequenced . ifoldMapOf l (\i -> Sequenced . liftM skip . f i)
{-# INLINE imapMOf_ #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @'iforMOf_' = 'flip' . 'imapMOf_'@
--
-- When you don't need access to the index then 'Control.Lens.Fold.forMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forMOf_' l a = 'iforMOf' l a . 'const'@
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i a c          -> a -> (i -> c -> m e) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i a c            -> a -> (i -> c -> m e) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> a -> (i -> c -> m e) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> a -> (i -> c -> m e) -> m ()
-- @
iforMOf_ :: Monad m => IndexedGetting i (Sequenced m) a c -> a -> (i -> c -> m e) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- |
-- Concatenate the results of a function of the elements of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- When you don't need access to the index then 'Control.Lens.Fold.concatMapOf'  is more flexible in what it accepts.
--
-- @
-- 'Control.Lens.Fold.concatMapOf' l = 'iconcatMapOf' l . 'const'
-- 'iconcatMapOf' = 'ifoldMapOf'
-- @
--
-- @
-- 'iconcatMapOf' :: 'IndexedGetter' i a c          -> (i -> c -> [e]) -> a -> [e]
-- 'iconcatMapOf' :: 'IndexedFold' i a c            -> (i -> c -> [e]) -> a -> [e]
-- 'iconcatMapOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> [e]) -> a -> [e]
-- 'iconcatMapOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> [e]) -> a -> [e]
-- @
iconcatMapOf :: IndexedGetting i [e] a c -> (i -> c -> [e]) -> a -> [e]
iconcatMapOf = ifoldMapOf
{-# INLINE iconcatMapOf #-}

-- | The 'findOf' function takes an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'Control.Lens.Fold.findOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.findOf' l = 'ifindOf' l . 'const'@
--
-- @
-- 'ifindOf' :: 'IndexedGetter' a c          -> (i -> c -> 'Bool') -> a -> 'Maybe' (i, c)
-- 'ifindOf' :: 'IndexedFold' a c            -> (i -> c -> 'Bool') -> a -> 'Maybe' (i, c)
-- 'ifindOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' a c      -> (i -> c -> 'Bool') -> a -> 'Maybe' (i, c)
-- 'ifindOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' a c -> (i -> c -> 'Bool') -> a -> 'Maybe' (i, c)
-- @
ifindOf :: IndexedGetting i (First (i, c)) a c -> (i -> c -> Bool) -> a -> Maybe (i, c)
ifindOf l p = getFirst . ifoldMapOf l step where
  step i c
    | p i c     = First $ Just (i, c)
    | otherwise = First Nothing
{-# INLINE ifindOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrOf'' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf'' l = 'ifoldrOf'' l . 'const'@
--
-- @
-- 'ifoldrOf'' :: 'IndexedGetter' i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- 'ifoldrOf'' :: 'IndexedFold' i a c            -> (i -> c -> e -> e) -> e -> a -> e
-- 'ifoldrOf'' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> e -> e) -> e -> a -> e
-- 'ifoldrOf'' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> e -> e) -> e -> a -> e
-- @
ifoldrOf' :: IndexedGetting i (Dual (Endo (e -> e))) a c -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlOf'' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf'' l = 'ifoldlOf'' l . 'const'@
--
-- @
-- 'ifoldlOf'' :: 'IndexedGetter' i a c            -> (i -> e -> c -> e) -> e -> a -> e
-- 'ifoldlOf'' :: 'IndexedFold' i a c              -> (i -> e -> c -> e) -> e -> a -> e
-- 'ifoldlOf'' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c        -> (i -> e -> c -> e) -> e -> a -> e
-- 'ifoldlOf'' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c   -> (i -> e -> c -> e) -> e -> a -> e
-- @
ifoldlOf' :: IndexedGetting i (Endo (e -> e)) a c -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldrMOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrMOf' l = 'ifoldrMOf' l . 'const'@
--
-- @
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedGetter' i a c          -> (i -> c -> e -> m e) -> e -> a -> e
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedFold' i a c            -> (i -> c -> e -> m e) -> e -> a -> e
-- 'ifoldrMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> (i -> c -> e -> m e) -> e -> a -> e
-- 'ifoldrMOf' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> (i -> c -> e -> m e) -> e -> a -> e
-- @
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (e -> m e))) a c -> (i -> c -> e -> m e) -> e -> a -> m e
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'Control.Lens.Fold.foldlMOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlMOf' l = 'ifoldlMOf' l . 'const'@
--
-- @
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedGetter' i a c            -> (i -> e -> c -> m e) -> e -> a -> e
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedFold' i a c              -> (i -> e -> c -> m e) -> e -> a -> e
-- 'ifoldlOf'' :: 'Monad' m => 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c        -> (i -> e -> c -> m e) -> e -> a -> e
-- 'ifoldlOf'' :: 'Monad' m => 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c   -> (i -> e -> c -> m e) -> e -> a -> e
-- @
ifoldlMOf :: Monad m => IndexedGetting i (Endo (e -> m e)) a c -> (i -> e -> c -> m e) -> e -> a -> m e
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'Control.Lens.Fold.toListOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.toListOf' l = 'map' 'fst' . 'itoListOf' l@
--
-- @
-- 'itoListOf' :: 'IndexedGetter' i a c          -> a -> [(i,c)]
-- 'itoListOf' :: 'IndexedFold' i a c            -> a -> [(i,c)]
-- 'itoListOf' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c      -> a -> [(i,c)]
-- 'itoListOf' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c -> a -> [(i,c)]
-- @
itoListOf :: IndexedGetting i [(i,c)] a c -> a -> [(i,c)]
itoListOf l = ifoldMapOf l (\i c -> [(i,c)])
{-# INLINE itoListOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Transform an indexed fold into a fold of both the indices and the values.
--
-- @
-- 'withIndices' :: 'IndexedFold' i a c             -> 'Fold' a (i, c)
-- 'withIndices' :: 'Simple' 'IndexedLens' i a c      -> 'Getter' a (i, c)
-- 'withIndices' :: 'Simple' 'IndexedTraversal' i a c -> 'Fold' a (i, c)
-- @
--
-- All 'Fold' operations are safe, and comply with the laws. However,
--
-- Passing this an 'IndexedTraversal' will still allow many
-- 'Traversal' combinators to type check on the result, but the result
-- can only be legally traversed by operations that do not edit the indices.
--
-- @
-- 'withIndices' :: 'IndexedTraversal' i a b c d -> 'Traversal' a b (i, c) (j, d)
-- @
--
-- Change made to the indices will be discarded.
withIndicesOf :: Functor f => Overloaded (Index i) f a b c d -> LensLike f a b (i, c) (j, d)
withIndicesOf l f = withIndex l (\i c -> snd <$> f (i,c))
{-# INLINE withIndicesOf #-}

-- | Transform an indexed fold into a fold of the indices.
--
-- @
-- 'indices' :: 'IndexedFold' i a c             -> 'Fold' a i
-- 'indices' :: 'Simple' 'IndexedLens' i a c      -> 'Getter' a i
-- 'indices' :: 'Simple' 'IndexedTraversal' i a c -> 'Fold' a i
-- @
indicesOf :: Gettable f => Overloaded (Index i) f a b c c -> LensLike f a b i j
indicesOf l f = withIndex l (const . coerce . f)
{-# INLINE indicesOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Obtain an 'IndexedFold' by filtering a 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter', 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'.
ifiltered :: (Gettable f, Applicative f, Indexed i k) => (i -> c -> Bool) -> Index i (c -> f c) (a -> f a) -> k (c -> f c) (a -> f a)
ifiltered p l = index $ \ f -> withIndex l $ \ i c -> if p i c then f i c else noEffect
{-# INLINE ifiltered #-}

-- | Obtain an 'IndexedFold' by taking elements from another 'IndexedFold', 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter' or 'Control.Lens.IndexedTraversal.IndexedTraversal' while a predicate holds.
itakingWhile :: (Gettable f, Applicative f, Indexed i k)
            => (i -> c -> Bool)
            -> IndexedGetting i (Endo (f a)) a c
            -> k (c -> f c) (a -> f a)
itakingWhile p l = index $ \ f -> ifoldrOf l (\i a r -> if p i a then f i a *> r else noEffect) noEffect
{-# INLINE itakingWhile #-}


-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'Control.Lens.IndexedLens.IndexedLens', 'IndexedGetter' or 'Control.Lens.IndexedTraversal.IndexedTraversal' while a predicate holds.
idroppingWhile :: (Gettable f, Applicative f, Indexed i k)
              => (i -> c -> Bool)
              -> IndexedGetting i (Endo (f a)) a c
              -> k (c -> f c) (a -> f a)
idroppingWhile p l = index $ \f -> ifoldrOf l (\i a r -> if p i a then r else f i a *> r) noEffect
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Reifying Indexed Folds
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedFold i a c = ReifyIndexedFold { reflectIndexedFold :: IndexedFold i a c }
