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
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.IndexedGetter
import Control.Lens.Internal
import Control.Monad
import Data.Monoid

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Every 'IndexedFold' is a valid 'Control.Lens.Fold.Fold'.
type IndexedFold i a c = forall k f b d. (Indexed i k, Applicative f, Gettable f) => k (c -> f d) (a -> f b)

-- |
-- Fold an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the index @i@.
--
-- When you don't need access to the index then 'foldMapOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldMapOf' l = 'ifoldMapOf' l . 'const'@
--
-- > ifoldMapOf ::             IndexedGetter i a c        -> (i -> c -> m) -> a -> m
-- > ifoldMapOf :: Monoid m => IndexedFold i a c          -> (i -> c -> m) -> a -> m
-- > ifoldMapOf ::             IndexedLens i a b c d      -> (i -> c -> m) -> a -> m
-- > ifoldMapOf :: Monoid m => IndexedTraversal i a b c d -> (i -> c -> m) -> a -> m
ifoldMapOf :: IndexedGetting i m a b c d -> (i -> c -> m) -> a -> m
ifoldMapOf l f = runAccessor . withIndex l (\i -> Accessor . f i)
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index @i@.
--
-- When you don't need access to the index then 'foldrOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf' l = 'ifoldrOf' l . 'const'@
--
-- > ifoldrOf :: IndexedGetter i a c        -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf :: IndexedFold i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf :: IndexedLens i a b c d      -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf :: IndexedTraversal i a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf :: IndexedGetting i (Endo e) a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf l f z t = appEndo (ifoldMapOf l (\i -> Endo . f i) t) z
{-# INLINE ifoldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index @i@.
--
-- When you don't need access to the index then 'foldlOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf' l = 'ifoldlOf' l . 'const'@
--
-- > ifoldlOf :: IndexedGetter i a c        -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf :: IndexedFold i a c          -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf :: IndexedLens i a b c d      -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf :: IndexedTraversal i a b c d -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf :: IndexedGetting i (Dual (Endo e)) a b c d -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf l f z t = appEndo (getDual (ifoldMapOf l (\i -> Dual . Endo . flip (f i)) t)) z
{-# INLINE ifoldlOf #-}

-- |
-- Return whether or not any element viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' 
-- satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'anyOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.anyOf' l = 'ianyOf' l . 'const'@
--
-- > ianyOf :: IndexedGetter i a c        -> (i -> c -> Bool) -> a -> Bool
-- > ianyOf :: IndexedFold i a c          -> (i -> c -> Bool) -> a -> Bool
-- > ianyOf :: IndexedLens i a b c d      -> (i -> c -> Bool) -> a -> Bool
-- > ianyOf :: IndexedTraversal i a b c d -> (i -> c -> Bool) -> a -> Bool
ianyOf :: IndexedGetting i Any a b c d -> (i -> c -> Bool) -> a -> Bool
ianyOf l f = getAny . ifoldMapOf l (\i -> Any . f i)
{-# INLINE ianyOf #-}

-- |
-- Return whether or not all elements viewed through an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' 
-- satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'allOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.allOf' l = 'iallOf' l . 'const'@
--
-- > iallOf :: IndexedGetter i a c        -> (i -> c -> Bool) -> a -> Bool
-- > iallOf :: IndexedFold i a c          -> (i -> c -> Bool) -> a -> Bool
-- > iallOf :: IndexedLens i a b c d      -> (i -> c -> Bool) -> a -> Bool
-- > iallOf :: IndexedTraversal i a b c d -> (i -> c -> Bool) -> a -> Bool
iallOf :: IndexedGetting i All a b c d -> (i -> c -> Bool) -> a -> Bool
iallOf l f = getAll . ifoldMapOf l (\i -> All . f i)
{-# INLINE iallOf #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index @i@, discarding the results.
--
-- When you don't need access to the index then 'traverseOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.traverseOf_' l = 'itraverseOf' l . 'const'@
--
-- > itraverseOf_ :: Functor f     => IndexedGetter i a c        -> (i -> c -> f e) -> a -> f ()
-- > itraverseOf_ :: Applicative f => IndexedFold i a c          -> (i -> c -> f e) -> a -> f ()
-- > itraverseOf_ :: Functor f     => IndexedLens i a b c d      -> (i -> c -> f e) -> a -> f ()
-- > itraverseOf_ :: Applicative f => IndexedTraversal i a b c d -> (i -> c -> f e) -> a -> f ()
itraverseOf_ :: Functor f => IndexedGetting i (Traversed f) a b c d -> (i -> c -> f e) -> a -> f ()
itraverseOf_ l f = getTraversed . ifoldMapOf l (\i -> Traversed . void . f i)
{-# INLINE itraverseOf_ #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @'iforOf_' = 'flip' . 'itraverseOf_'@
--
-- When you don't need access to the index then 'forOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forOf_' l a = 'iforOf' l a . 'const'@
--
-- > iforOf_ :: Functor f     => IndexedGetter i a c        -> a -> (i -> c -> f e) -> f ()
-- > iforOf_ :: Applicative f => IndexedFold i a c          -> a -> (i -> c -> f e) -> f ()
-- > iforOf_ :: Functor f     => IndexedLens i a b c d      -> a -> (i -> c -> f e) -> f ()
-- > iforOf_ :: Applicative f => IndexedTraversal i a b c d -> a -> (i -> c -> f e) -> f ()
iforOf_ :: Functor f => IndexedGetting i (Traversed f) a b c d -> a -> (i -> c -> f e) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'mapMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.mapMOf_' l = 'imapMOf' l . 'const'@
--
-- > imapMOf_ :: Monad m => IndexedGetter i a c        -> (i -> c -> m e) -> a -> m ()
-- > imapMOf_ :: Monad m => IndexedFold i a c          -> (i -> c -> m e) -> a -> m ()
-- > imapMOf_ :: Monad m => IndexedLens i a b c d      -> (i -> c -> m e) -> a -> m ()
-- > imapMOf_ :: Monad m => IndexedTraversal i a b c d -> (i -> c -> m e) -> a -> m ()
imapMOf_ :: Monad m => IndexedGetting i (Sequenced m) a b c d -> (i -> c -> m e) -> a -> m ()
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
-- When you don't need access to the index then 'forMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forMOf_' l a = 'iforMOf' l a . 'const'@
--
-- > iforMOf_ :: Monad m => IndexedGetter i a c        -> a -> (i -> c -> m e) -> m ()
-- > iforMOf_ :: Monad m => IndexedFold i a c          -> a -> (i -> c -> m e) -> m ()
-- > iforMOf_ :: Monad m => IndexedLens i a b c d      -> a -> (i -> c -> m e) -> m ()
-- > iforMOf_ :: Monad m => IndexedTraversal i a b c d -> a -> (i -> c -> m e) -> m ()
iforMOf_ :: Monad m => IndexedGetting i (Sequenced m) a b c d -> a -> (i -> c -> m e) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- |
-- Concatenate the results of a function of the elements of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal'
-- with access to the index.
--
-- When you don't need access to the index then 'concatMapOf_'  is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.concatMapOf_' l = 'iconcatMapMOf' l . 'const'@
--
-- > iconcatMapOf :: IndexedGetter i a c        -> (i -> c -> [e]) -> a -> [e]
-- > iconcatMapOf :: IndexedFold i a c          -> (i -> c -> [e]) -> a -> [e]
-- > iconcatMapOf :: IndexedLens i a b c d      -> (i -> c -> [e]) -> a -> [e]
-- > iconcatMapOf :: IndexedTraversal i a b c d -> (i -> c -> [e]) -> a -> [e]
iconcatMapOf :: IndexedGetting i [e] a b c d -> (i -> c -> [e]) -> a -> [e]
iconcatMapOf l ices = runAccessor . withIndex l (\i -> Accessor . ices i)
{-# INLINE iconcatMapOf #-}

-- | The 'findOf' function takes an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.findOf' l = 'ifoldOf' l . 'const'@
--
-- > ifindOf :: IndexedGetter a c        -> (i -> c -> Bool) -> a -> Maybe (i, c)
-- > ifindOf :: IndexedFold a c          -> (i -> c -> Bool) -> a -> Maybe (i, c)
-- > ifindOf :: IndexedLens a b c d      -> (i -> c -> Bool) -> a -> Maybe (i, c)
-- > ifindOf :: IndexedTraversal a b c d -> (i -> c -> Bool) -> a -> Maybe (i, c)
ifindOf :: IndexedGetting i (First (i, c)) a b c d -> (i -> c -> Bool) -> a -> Maybe (i, c)
ifindOf l p = getFirst . ifoldMapOf l step where
  step i c
    | p i c     = First $ Just (i, c)
    | otherwise = First Nothing
{-# INLINE ifindOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrOf'' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrOf'' l = 'ifoldrOf'' l . 'const'@
--
-- > ifoldrOf' :: IndexedGetter i a c        -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf' :: IndexedFold i a c          -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf' :: IndexedLens i a b c d      -> (i -> c -> e -> e) -> e -> a -> e
-- > ifoldrOf' :: IndexedTraversal i a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf' :: IndexedGetting i (Dual (Endo (e -> e))) a b c d -> (i -> c -> e -> e) -> e -> a -> e
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'foldlOf'' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlOf'' l = 'ifoldlOf'' l . 'const'@
--
-- > ifoldlOf' :: IndexedGetter i a c          -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf' :: IndexedFold i a c            -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf' :: IndexedLens i a b c d        -> (i -> e -> c -> e) -> e -> a -> e
-- > ifoldlOf' :: IndexedTraversal i a b c d   -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf' :: IndexedGetting i (Endo (e -> e)) a b c d -> (i -> e -> c -> e) -> e -> a -> e
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrMOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldrMOf' l = 'ifoldrMOf' l . 'const'@
--
-- > ifoldrMOf :: Monad m => IndexedGetter i a c        -> (i -> c -> e -> m e) -> e -> a -> e
-- > ifoldrMOf :: Monad m => IndexedFold i a c          -> (i -> c -> e -> m e) -> e -> a -> e
-- > ifoldrMOf :: Monad m => IndexedLens i a b c d      -> (i -> c -> e -> m e) -> e -> a -> e
-- > ifoldrMOf :: Monad m => IndexedTraversal i a b c d -> (i -> c -> e -> m e) -> e -> a -> e
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (e -> m e))) a b c d -> (i -> c -> e -> m e) -> e -> a -> m e
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlMOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.foldlMOf' l = 'ifoldlMOf' l . 'const'@
--
-- > ifoldlOf' :: Monad m => IndexedGetter i a c          -> (i -> e -> c -> m e) -> e -> a -> e
-- > ifoldlOf' :: Monad m => IndexedFold i a c            -> (i -> e -> c -> m e) -> e -> a -> e
-- > ifoldlOf' :: Monad m => IndexedLens i a b c d        -> (i -> e -> c -> m e) -> e -> a -> e
-- > ifoldlOf' :: Monad m => IndexedTraversal i a b c d   -> (i -> e -> c -> m e) -> e -> a -> e
ifoldlMOf :: Monad m => IndexedGetting i (Endo (e -> m e)) a b c d -> (i -> e -> c -> m e) -> e -> a -> m e
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices, then 'toListOf' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.toListOf' l = 'map' 'fst' . 'itoListOf' l@
--
-- > itoListOf :: IndexedGetter i a c        -> a -> [(i,c)]
-- > itoListOf :: IndexedFold i a c          -> a -> [(i,c)]
-- > itoListOf :: IndexedLens i a b c d      -> a -> [(i,c)]
-- > itoListOf :: IndexedTraversal i a b c d -> a -> [(i,c)]
itoListOf :: IndexedGetting i [(i,c)] a b c d -> a -> [(i,c)]
itoListOf l = ifoldMapOf l (\i c -> [(i,c)])
{-# INLINE itoListOf #-}
