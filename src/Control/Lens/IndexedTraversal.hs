{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedTraversal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.IndexedTraversal
  (
  -- * Indexed Traversals
    IndexedTraversal

  -- * Common Indexed Traversals
  , iwhereOf
  , value
  , ignored
  , At(..)
  , TraverseMin(..)
  , TraverseMax(..)
  , traversed
  , traversed64
  , elementOf
  , element
  , elementsOf
  , elements

  -- * Indexed Traversal Combinators
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Storing Indexed Traversals
  , ReifiedIndexedTraversal(..)

  -- * Simple
  , SimpleIndexedTraversal
  , SimpleReifiedIndexedTraversal
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Combinators
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.Trans.State.Lazy as Lazy
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.Int
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Traversable

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Every indexed traversal is a valid 'Control.Lens.Traversal.Traversal' or
-- 'Control.Lens.IndexedFold.IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'IndexedTraversal' to be used
-- directly as a 'Control.Lens.Traversal.Traversal'.
--
-- The 'Control.Lens.Traversal.Traversal' laws are still required to hold.
type IndexedTraversal i s t a b = forall f k. (Indexable i k, Applicative f) => k (a -> f b) (s -> f t)

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i s a = IndexedTraversal i s s a a

-- | Traversal with an index.
--
-- NB: When you don't need access to the index then you can just apply your 'IndexedTraversal'
-- directly as a function!
--
-- @
-- 'itraverseOf' ≡ 'withIndex'
-- 'Control.Lens.Traversal.traverseOf' l = 'itraverseOf' l '.' 'const' = 'id'
-- @
--
-- @
-- 'itraverseOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- 'itraverseOf' :: 'IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
itraverseOf :: Overloaded (Indexed i) f s t a b -> (i -> a -> f b) -> s -> f t
itraverseOf = withIndex
{-# INLINE itraverseOf #-}

-- |
-- Traverse with an index (and the arguments flipped)
--
-- @
-- 'Control.Lens.Traversal.forOf' l a ≡ 'iforOf' l a '.' 'const'
-- 'iforOf' ≡ 'flip' . 'itraverseOf'
-- @
--
-- @
-- 'iforOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> s -> (i -> a -> f b) -> f t
-- 'iforOf' :: 'IndexedTraversal' i s t a b -> s -> (i -> a -> f b) -> f t
-- @
iforOf :: Overloaded (Indexed i) f s t a b -> s -> (i -> a -> f b) -> f t
iforOf = flip . withIndex
{-# INLINE iforOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- When you don't need access to the index 'mapMOf' is more liberal in what it can accept.
--
-- @'Control.Lens.Traversal.mapMOf' l ≡ 'imapMOf' l '.' 'const'@
--
-- @
-- 'imapMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens'      i s t a b -> (i -> a -> m b) -> s -> m t
-- 'imapMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> (i -> a -> m b) -> s -> m t
-- @
imapMOf :: Overloaded (Indexed i) (WrappedMonad m) s t a b -> (i -> a -> m b) -> s -> m t
imapMOf l f = unwrapMonad . withIndex l (\i -> WrapMonad . f i)
{-# INLINE imapMOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'Control.Lens.Traversal.forMOf' l a ≡ 'iforMOf' l a '.' 'const'
-- 'iforMOf' ≡ 'flip' '.' 'imapMOf'
-- @
--
-- @
-- 'iforMOf' :: 'Monad' m => 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> s -> (i -> a -> m b) -> m t
-- 'iforMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> s -> (i -> a -> m b) -> m t
-- @
iforMOf :: Overloaded (Indexed i) (WrappedMonad m) s t a b -> s -> (i -> a -> m b) -> m t
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @'Control.Lens.Traversal.mapAccumROf' l ≡ 'imapAccumROf' l '.' 'const'@
--
-- @
-- 'imapAccumROf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- 'imapAccumROf' :: 'IndexedTraversal' i s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- @
imapAccumROf :: Overloaded (Indexed i) (Lazy.State s) s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
imapAccumROf l f s0 a = swap (Lazy.runState (withIndex l (\i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @'Control.Lens.Traversal.mapAccumLOf' l ≡ 'imapAccumLOf' l '.' 'const'@
--
-- @
-- 'imapAccumLOf' :: 'Control.Lens.IndexedLens.IndexedLens' i s t a b      -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- 'imapAccumLOf' :: 'IndexedTraversal' i s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- @
imapAccumLOf :: Overloaded (Indexed i) (Backwards (Lazy.State s)) s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
imapAccumLOf l f s0 a = swap (Lazy.runState (forwards (withIndex l (\i c -> Backwards (Lazy.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

------------------------------------------------------------------------------
-- Common Indexed Traversals
------------------------------------------------------------------------------

-- | Access the element of an 'IndexedTraversal' where the index matches a predicate.
--
-- >>> over (iwhereOf traversed (>0)) reverse ["He","was","stressed","o_O"]
-- ["He","saw","desserts","O_o"]
--
-- @
-- 'iwhereOf' :: 'Control.Lens.IndexedFold.IndexedFold' i s a            -> (i -> 'Bool') -> 'Control.Lens.IndexedFold.IndexedFold' i s a
-- 'iwhereOf' :: 'IndexedGetter' i s a          -> (i -> 'Bool') -> 'Control.Lens.IndexedFold.IndexedFold' i s a
-- 'iwhereOf' :: 'SimpleIndexedLens' i s a      -> (i -> 'Bool') -> 'SimpleIndexedTraversal' i s a
-- 'iwhereOf' :: 'SimpleIndexedTraversal' i s a -> (i -> 'Bool') -> 'SimpleIndexedTraversal' i s a
-- 'iwhereOf' :: 'SimpleIndexedSetter' i s a    -> (i -> 'Bool') -> 'SimpleIndexedSetter' i s a
-- @
iwhereOf :: (Indexable i k, Applicative f) => Overloaded (Indexed i) f s t a a -> (i -> Bool) -> Overloaded k f s t a a
iwhereOf l p = indexed $ \f s -> withIndex l (\i a -> if p i then f i a else pure a) s
{-# INLINE iwhereOf #-}

-- | Traverse any 'Traversable' container. This is an 'IndexedTraversal' that is indexed by ordinal position.
traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b
traversed = indexing traverse
{-# INLINE traversed #-}

-- | Traverse any 'Traversable' container. This is an 'IndexedTraversal' that is indexed by ordinal position.
traversed64 :: Traversable f => IndexedTraversal Int64 (f a) (f b) a b
traversed64 = indexing64 traverse
{-# INLINE traversed64 #-}

-- | This provides a 'Control.Lens.Traversal.Traversal' that checks a predicate on a key before
-- allowing you to traverse into a value.
value :: (k -> Bool) -> SimpleIndexedTraversal k (k, v) v
value p = indexed $ \ f kv@(k,v) -> if p k then (,) k <$> f k v else pure kv
{-# INLINE value #-}

-- | This is the trivial empty traversal.
--
-- @'ignored' :: 'IndexedTraversal' i s s a b@
--
-- @'ignored' ≡ 'const' 'pure'@
ignored :: forall k f i s a b. (Indexable i k, Applicative f) => Overloaded k f s s a b
ignored = indexed $ \ (_ :: i -> a -> f b) s -> pure s :: f s
{-# INLINE ignored #-}

-- | 'At' provides a lens that can be used to read,
-- write or delete the value associated with a key in a map-like
-- container on an ad hoc basis.
class At k m | m -> k where
  -- |
  -- >>> Map.fromList [(1,"hello")] ^.at 1
  -- Just "hello"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- Note: 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at :: k -> SimpleIndexedLens k (m v) (Maybe v)

  -- | This simple indexed traversal lets you 'traverse' the value at a given key in a map.
  --
  -- *NB:* _setting_ the value of this lens will only set the value in the lens
  -- if it is already present.
  --
  -- @'_at' k ≡ 'at' k '<.' 'traverse'@
  _at :: k -> SimpleIndexedTraversal k (m v) v
  _at k = at k <. traverse

instance At Int IntMap where
  at k = indexed $ \f m ->
    let mv = IntMap.lookup k m
        go Nothing   = maybe m (const (IntMap.delete k m)) mv
        go (Just v') = IntMap.insert k v' m
    in go <$> f k mv where
  {-# INLINE at #-}
  _at k = indexed $ \f m -> case IntMap.lookup k m of
     Just v -> f k v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE _at #-}

instance Ord k => At k (Map k) where
  at k = indexed $ \f m ->
    let mv = Map.lookup k m
        go Nothing   = maybe m (const (Map.delete k m)) mv
        go (Just v') = Map.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}
  _at k = indexed $ \f m -> case Map.lookup k m of
     Just v  -> f k v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE _at #-}

instance (Eq k, Hashable k) => At k (HashMap k) where
  at k = indexed $ \f m ->
    let mv = HashMap.lookup k m
        go Nothing   = maybe m (const (HashMap.delete k m)) mv
        go (Just v') = HashMap.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}
  _at k = indexed $ \f m -> case HashMap.lookup k m of
     Just v  -> f k v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE _at #-}

-- | Allows 'IndexedTraversal' the value at the smallest index.
class Ord k => TraverseMin k m | m -> k where
  -- | 'IndexedTraversal' of the element with the smallest index.
  traverseMin :: SimpleIndexedTraversal k (m v) v

instance TraverseMin Int IntMap where
  traverseMin = indexed $ \f m -> case IntMap.minViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> f k a <&> \v -> IntMap.updateMin (const (Just v)) m
#else
    Just ((k,a), _) -> f k a <&> \v -> IntMap.updateMin (const v) m
#endif
    Nothing     -> pure m
  {-# INLINE traverseMin #-}

instance Ord k => TraverseMin k (Map k) where
  traverseMin = indexed $ \f m -> case Map.minViewWithKey m of
    Just ((k, a), _) -> f k a <&> \v -> Map.updateMin (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMin #-}

-- | Allows 'IndexedTraversal' of the value at the largest index.
class Ord k => TraverseMax k m | m -> k where
  -- | 'IndexedTraversal' of the element at the largest index.
  traverseMax :: SimpleIndexedTraversal k (m v) v

instance TraverseMax Int IntMap where
  traverseMax = indexed $ \f m -> case IntMap.maxViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> f k a <&> \v -> IntMap.updateMax (const (Just v)) m
#else
    Just ((k,a), _) -> f k a <&> \v -> IntMap.updateMax (const v) m
#endif
    Nothing     -> pure m
  {-# INLINE traverseMax #-}

instance Ord k => TraverseMax k (Map k) where
  traverseMax = indexed $ \f m -> case Map.maxViewWithKey m of
    Just ((k, a), _) -> f k a <&> \v -> Map.updateMax (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMax #-}

-- | Traverse the /nth/ element 'elementOf' a 'Control.Lens.Traversal.Traversal', 'Lens' or
-- 'Control.Lens.Iso.Iso' if it exists.
--
-- >>> [[1],[3,4]] & elementOf (traverse.traverse) 1 .~ 5
-- [[1],[5,4]]
--
-- >>> [[1],[3,4]] ^? elementOf (folded.folded) 1
-- Just 3
--
-- >>> [0..] ^?! elementOf folded 5
-- 5
--
-- >>> take 10 $ elementOf traverse 3 .~ 16 $ [0..]
-- [0,1,2,16,4,5,6,7,8,9]
--
-- @
-- 'elementOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> Int -> 'SimpleIndexedTraversal' 'Int' s a
-- 'elementOf' :: 'Control.Lens.Fold.Fold' s a            -> Int -> 'Control.Lens.IndexedFold.IndexedFold' 'Int' s a
-- @
elementOf :: (Applicative f, Indexable Int k)
          => LensLike (Indexing f) s t a a
          -> Int
          -> Overloaded k f s t a a
elementOf l p = elementsOf l (p ==)
{-# INLINE elementOf #-}

-- | Traverse the /nth/ element of a 'Traversable' container.
--
-- @'element' ≡ 'elementOf' 'traverse'@
element :: Traversable t => Int -> SimpleIndexedTraversal Int (t a) a
element = elementOf traverse
{-# INLINE element #-}

-- | Traverse (or fold) selected elements of a 'Control.Lens.Traversal.Traversal' (or 'Control.Lens.Fold.Fold') where their ordinal positions match a predicate.
--
-- @
-- 'elementsOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> ('Int' -> 'Bool') -> 'SimpleIndexedTraversal' 'Int' s a
-- 'elementsOf' :: 'Control.Lens.Fold.Fold' s a            -> ('Int' -> 'Bool') -> 'Control.Lens.IndexedFold.IndexedFold' 'Int' s a
-- @
elementsOf :: (Applicative f, Indexable Int k)
           => LensLike (Indexing f) s t a a
           -> (Int -> Bool)
           -> Overloaded k f s t a a
elementsOf l p = indexed $ \iafb s ->
  case runIndexing (l (\a -> Indexing (\i -> i `seq` (if p i then iafb i a else pure a, i + 1))) s) 0 of
    (r, _) -> r
{-# INLINE elementsOf #-}

-- | Traverse elements of a 'Traversable' container where their ordinal positions matches a predicate.
--
-- @'elements' ≡ 'elementsOf' 'traverse'@
elements :: Traversable t => (Int -> Bool) -> SimpleIndexedTraversal Int (t a) a
elements = elementsOf traverse
{-# INLINE elements #-}

------------------------------------------------------------------------------
-- Reifying Indexed Traversals
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedTraversal i s t a b =
  ReifyIndexedTraversal { reflectIndexedTraversal :: IndexedTraversal i s t a b }

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('ReifiedIndexedTraversal' i)@
type SimpleReifiedIndexedTraversal i s a = ReifiedIndexedTraversal i s s a a
