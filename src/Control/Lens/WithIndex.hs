{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 706
{-# LANGUAGE DefaultSignatures #-}
#define MPTC_DEFAULTS
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.WithIndex
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- (These need to be defined together for @DefaultSignatures@ to work.)
-------------------------------------------------------------------------------
module Control.Lens.WithIndex
  (
  -- * Indexed Functors
    FunctorWithIndex(..)
  , imapped
  -- * Indexed Foldables
  , FoldableWithIndex(..)
  , ifolded
  , ifolding
  -- ** Indexed Foldable Combinators
  , iany
  , iall
  , itraverse_
  , ifor_
  , imapM_
  , iforM_
  , iconcatMap
  , ifind
  , ifoldrM
  , ifoldlM
  , itoList
  -- * Converting to Folds
  , withIndices
  , indices
  -- * Indexed Traversables
  , TraversableWithIndex(..)
  , itraversed
  -- * Indexed Traversable Combinators
  , ifor
  , imapM
  , iforM
  , imapAccumR
  , imapAccumL
  , iwhere
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad (void, liftM)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Lens.Classes
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Internal.Combinators
import Control.Lens.Indexed
import Control.Lens.IndexedSetter
import Control.Lens.IndexedFold
import Control.Lens.IndexedTraversal
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Monoid
import Data.Sequence hiding (index)
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V

-- $setup
-- >>> import Control.Lens

-------------------------------------------------------------------------------
-- FunctorWithIndex
-------------------------------------------------------------------------------

-- | A 'Functor' with an additional index.
--
-- Instances must satisfy a modified form of the 'Functor' laws:
--
-- @
-- 'imap' f '.' 'imap' g ≡ 'imap' (\i -> f i . g i)
-- 'imap' (\_ a -> a) ≡ 'id'
-- @
class Functor f => FunctorWithIndex i f | f -> i where
  -- | Map with access to the index.
  imap :: (i -> a -> b) -> f a -> f b
#ifdef MPTC_DEFAULTS
  default imap :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
  imap = imapOf itraversed
#endif

-- | The 'IndexedSetter' for a 'FunctorWithIndex'.
--
-- If you don't need access to the index, then 'mapped' is more flexible in what it accepts.
imapped :: FunctorWithIndex i f => IndexedSetter i (f a) (f b) a b
imapped = isets imap
{-# INLINE imapped #-}

-------------------------------------------------------------------------------
-- FoldableWithIndex
-------------------------------------------------------------------------------

-- | A container that supports folding with an additional index.
class Foldable f => FoldableWithIndex i f | f -> i where
  --
  -- |
  -- Fold a container by mapping value to an arbitrary 'Monoid' with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldMap' is more flexible in what it accepts.
  --
  -- @'foldMap' ≡ 'ifoldMap' '.' 'const'@
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m
#ifdef MPTC_DEFAULTS
  default ifoldMap :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
  ifoldMap = ifoldMapOf itraversed
  {-# INLINE ifoldMap #-}
#endif

  -- | Right-associative fold of an indexed container with access to the index @i@.
  --
  -- When you don't need access to the index then 'Data.Foldable.foldr' is more flexible in what it accepts.
  --
  -- @'Data.Foldable.foldr' ≡ 'ifoldr' '.' 'const'@
  ifoldr   :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr f z t = appEndo (ifoldMap (\i -> endo# (f i)) t) z

  -- |
  -- Left-associative fold of an indexed container with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldl' is more flexible in what it accepts.
  --
  -- @'foldl' ≡ 'ifoldl' '.' 'const'@
  ifoldl :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl f z t = appEndo (getDual (ifoldMap (\i -> dual# (endo# (flip (f i)))) t)) z

  -- | /Strictly/ fold right over the elements of a structure with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldr'' is more flexible in what it accepts.
  --
  -- @'foldr'' ≡ 'ifoldr'' '.' 'const'@
  ifoldr' :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr' f z0 xs = ifoldl f' id xs z0
    where f' i k x z = k $! f i x z

  -- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
  --
  -- When you don't need access to the index then 'Control.Lens.Fold.foldlOf'' is more flexible in what it accepts.
  --
  -- @'Control.Lens.Fold.foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'@
  --
  -- @
  -- 'ifoldlOf'' :: 'Control.Lens.IndexedGetter.IndexedGetter' i a c            -> (i -> e -> c -> e) -> e -> a -> e
  -- 'ifoldlOf'' :: 'IndexedFold' i a c              -> (i -> e -> c -> e) -> e -> a -> e
  -- 'ifoldlOf'' :: 'Control.Lens.IndexedLens.SimpleIndexedLens' i a c        -> (i -> e -> c -> e) -> e -> a -> e
  -- 'ifoldlOf'' :: 'Control.Lens.IndexedTraversal.SimpleIndexedTraversal' i a c   -> (i -> e -> c -> e) -> e -> a -> e
  -- @
  ifoldl' :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl' f z0 xs = ifoldr f' id xs z0
    where f' i x k z = k $! f i z x

-- | The 'IndexedFold' of a 'FoldableWithIndex' container.
ifolded :: FoldableWithIndex i f => IndexedFold i (f a) a
ifolded = index $ \ f -> coerce . getFolding . ifoldMap (\i -> folding# (f i))
{-# INLINE ifolded #-}

-- | Obtain a 'Fold' by lifting an operation that returns a foldable result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
ifolding :: FoldableWithIndex i f => (a -> f c) -> IndexedFold i a c
ifolding afc = index $ \ icgd -> coerce . itraverse_ icgd . afc
{-# INLINE ifolding #-}

-- |
-- Return whether or not any element in a container satisfies a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'any' is more flexible in what it accepts.
--
-- @'any' = 'iany' '.' 'const'@
iany :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
iany f = getAny# (ifoldMap (\i -> any# (f i)))
{-# INLINE iany #-}

-- |
-- Return whether or not all elements in a container satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'all' is more flexible in what it accepts.
--
-- @'all' ≡ 'iall' '.' 'const'@
iall :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
iall f = getAll# (ifoldMap (\i -> all# (f i)))
{-# INLINE iall #-}

-- |
-- Traverse elements with access to the index @i@, discarding the results.
--
-- When you don't need access to the index then 'traverse_' is more flexible in what it accepts.
--
-- @'traverse_' l = 'itraverse' '.' 'const'@
itraverse_ :: (FoldableWithIndex i t, Applicative f) => (i -> a -> f b) -> t a -> f ()
itraverse_ f = getTraversed# (ifoldMap (\i -> traversed# (void . f i)))
{-# INLINE itraverse_ #-}

-- |
-- Traverse elements with access to the index @i@, discarding the results (with the arguments flipped).
--
-- @'ifor_' ≡ 'flip' 'itraverse_'@
--
-- When you don't need access to the index then 'for_' is more flexible in what it accepts.
--
-- @'for_' a ≡ 'ifor_' a '.' 'const'@
ifor_ :: (FoldableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f ()
ifor_ = flip itraverse_
{-# INLINE ifor_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.mapMOf_' is more flexible in what it accepts.
--
-- @'mapM_' ≡ 'imapM' '.' 'const'@
imapM_ :: (FoldableWithIndex i t, Monad m) => (i -> a -> m b) -> t a -> m ()
imapM_ f = getSequenced# (ifoldMap (\i -> sequenced# (liftM skip . f i)))
{-# INLINE imapM_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @'iforM_' ≡ 'flip' 'imapM_'@
--
-- When you don't need access to the index then 'Control.Lens.Fold.forMOf_' is more flexible in what it accepts.
--
-- @'Control.Lens.Fold.forMOf_' l a ≡ 'iforMOf' l a '.' 'const'@
iforM_ :: (FoldableWithIndex i t, Monad m) => t a -> (i -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}

-- |
-- Concatenate the results of a function of the elements of an indexed container with access to the index.
--
-- When you don't need access to the index then 'concatMap' is more flexible in what it accepts.
--
-- @
-- 'concatMap' ≡ 'iconcatMap' . 'const'
-- 'iconcatMap' ≡ 'ifoldMap'
-- @
iconcatMap :: FoldableWithIndex i f => (i -> a -> [b]) -> f a -> [b]
iconcatMap = ifoldMap
{-# INLINE iconcatMap #-}

-- | Searches a container with a predicate that is also supplied the index, returning the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'find' is more flexible in what it accepts.
--
-- @'find' ≡ 'ifind' '.' 'const'@
ifind :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Maybe (i, a)
ifind p = getFirst . ifoldMap step where
  step i c
    | p i c     = First $ Just (i, c)
    | otherwise = First Nothing
{-# INLINE ifind #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrM' is more flexible in what it accepts.
--
-- @'foldrM' ≡ 'ifoldrM' '.' 'const'@
ifoldrM :: (FoldableWithIndex i f, Monad m) => (i -> a -> b -> m b) -> b -> f a -> m b
ifoldrM f z0 xs = ifoldl f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrM #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlM' is more flexible in what it accepts.
--
-- @'foldlM' ≡ 'ifoldlM' '.' 'const'@
ifoldlM :: (FoldableWithIndex i f, Monad m) => (i -> b -> a -> m b) -> b -> f a -> m b
ifoldlM f z0 xs = ifoldr f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlM #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'toList' is more flexible in what it accepts.
--
-- @'toList' ≡ 'map' 'fst' '.' 'itoList'@
itoList :: FoldableWithIndex i f => f a -> [(i,a)]
itoList = ifoldr (\i c -> ((i,c):)) []
{-# INLINE itoList #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Fold a container with indices returning both the indices and the values.
withIndices :: FoldableWithIndex i f => Fold (f a) (i,a)
withIndices f = coerce . getFolding . ifoldMap (\i a -> Folding (f (i,a)))
{-# INLINE withIndices #-}

-- | Fold a container with indices returning only the indices.
indices :: FoldableWithIndex i f => Fold (f a) i
indices f = coerce . getFolding# (ifoldMap (const . folding# f))
{-# INLINE indices #-}

-------------------------------------------------------------------------------
-- TraversableWithIndex
-------------------------------------------------------------------------------

-- | A 'Traversable' with an additional index.
--
-- An instance must satisfy a (modified) form of the 'Traversable' laws:
--
-- @
-- 'itraverse' ('const' 'Data.Functor.Identity.Identity') ≡ 'Data.Functor.Identity.Identity'
-- 'fmap' ('itraverse' f) '.' 'itraverse' g ≡ 'getCompose' '.' 'itraverse' (\i -> 'Compose' '.' 'fmap' (f i) '.' g i)
-- @
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where
  -- | Traverse an indexed container.
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)
#ifdef MPTC_DEFAULTS
  default itraverse :: Applicative f => (Int -> a -> f b) -> t a -> f (t b)
  itraverse = withIndex (indexed traverse)
  {-# INLINE itraverse #-}
#endif

-- | The 'IndexedTraversal' of a 'TraversableWithIndex' container.
itraversed :: TraversableWithIndex i f => IndexedTraversal i (f a) (f b) a b
itraversed = index itraverse
{-# INLINE itraversed #-}

-- |
-- Traverse with an index (and the arguments flipped)
--
-- @
-- 'for' a ≡ 'ifor' a '.' 'const'
-- 'ifor' ≡ 'flip' 'itraverse'
-- @
ifor :: (TraversableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f (t b)
ifor = flip itraverse
{-# INLINE ifor #-}

-- | Map each element of a structure to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- the index.
--
-- When you don't need access to the index 'mapM' is more liberal in what it can accept.
--
-- @'mapM' ≡ 'imapM' '.' 'const'@
imapM :: (TraversableWithIndex i t, Monad m) => (i -> a -> m b) -> t a -> m (t b)
imapM f = unwrapMonad# (itraverse (\i -> wrapMonad# (f i)))
{-# INLINE imapM #-}

-- | Map each element of a structure to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'forM' a ≡ 'iforM' a '.' 'const'
-- 'iforM' ≡ 'flip' 'imapM'
-- @
iforM :: (TraversableWithIndex i t, Monad m) => t a -> (i -> a -> m b) -> m (t b)
iforM = flip imapM
{-# INLINE iforM #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to add access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @'Control.Lens.Traversal.mapAccumR' ≡ 'imapAccumR' '.' 'const'@
imapAccumR :: TraversableWithIndex i t => (i -> s -> a -> (s, b)) -> s -> t a -> (s, t b)
imapAccumR f s0 a = swap (Lazy.runState (itraverse (\i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumR #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to add access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @'Control.Lens.Traversal.mapAccumLOf' ≡ 'imapAccumL' '.' 'const'@
imapAccumL :: TraversableWithIndex i t => (i -> s -> a -> (s, b)) -> s -> t a -> (s, t b)
imapAccumL f s0 a = swap (Lazy.runState (forwards (itraverse (\i c -> Backwards (Lazy.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumL #-}

-- | Access the element of an indexed container where the index matches a predicate.
--
-- >>> over (iwhere (>0)) Prelude.reverse $ ["He","was","stressed","o_O"]
-- ["He","saw","desserts","O_o"]
iwhere :: (TraversableWithIndex i t) => (i -> Bool) -> SimpleIndexedTraversal i (t a) a
iwhere p = index $ \f a -> itraverse (\i c -> if p i then f i c else pure c) a
{-# INLINE iwhere #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | The position in the list is available as the index.
instance FunctorWithIndex Int [] where
  imap = imapOf itraversed
instance FoldableWithIndex Int [] where
  ifoldMap = ifoldMapOf itraversed
instance TraversableWithIndex Int [] where
  itraverse = withIndex (indexed traverse)

-- | The position in the sequence is available as the index.
instance FunctorWithIndex Int Seq where
  imap = imapOf itraversed
instance FoldableWithIndex Int Seq where
  ifoldMap = ifoldMapOf itraversed
instance TraversableWithIndex Int Seq where
  itraverse = withIndex (indexed traverse)

instance FunctorWithIndex Int Vector where
  imap = V.imap
instance FoldableWithIndex Int Vector where
  ifoldMap = ifoldMapOf itraversed
  ifoldr = V.ifoldr
  ifoldl = V.ifoldl . flip
  ifoldr' = V.ifoldr'
  ifoldl' = V.ifoldl' . flip
instance TraversableWithIndex Int Vector where
  itraverse f = sequenceA . V.imap f

instance FunctorWithIndex Int IntMap where
  imap = imapOf itraversed
instance FoldableWithIndex Int IntMap where
  ifoldMap = ifoldMapOf itraversed
instance TraversableWithIndex Int IntMap where
#if MIN_VERSION_containers(0,5,0)
  itraverse = IntMap.traverseWithKey
#else
  itraverse f = sequenceA . IntMap.mapWithKey f
#endif
  {-# INLINE itraverse #-}

instance Ord k => FunctorWithIndex k (Map k) where
  imap = imapOf itraversed
instance Ord k => FoldableWithIndex k (Map k) where
  ifoldMap = ifoldMapOf itraversed
instance Ord k => TraversableWithIndex k (Map k) where
#if MIN_VERSION_containers(0,5,0)
  itraverse = Map.traverseWithKey
#else
  itraverse f = sequenceA . Map.mapWithKey f
#endif
  {-# INLINE itraverse #-}

instance (Eq k, Hashable k) => FunctorWithIndex k (HashMap k) where
  imap = imapOf itraversed
instance (Eq k, Hashable k) => FoldableWithIndex k (HashMap k) where
  ifoldMap = ifoldMapOf itraversed
instance (Eq k, Hashable k) => TraversableWithIndex k (HashMap k) where
  itraverse = HashMap.traverseWithKey
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}
