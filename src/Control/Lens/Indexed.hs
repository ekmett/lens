{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-} -- vector, hashable
#endif

#include "lens-common.h"

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Indexed
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- (The classes in here need to be defined together for @DefaultSignatures@ to work.)
-------------------------------------------------------------------------------
module Control.Lens.Indexed
  (
  -- * Indexing
    Indexable(..)
  , Conjoined(..)
  , Indexed(..)
  , (<.), (<.>), (.>)
  , selfIndex
  , reindexed
  , icompose
  , indexing
  , indexing64
  -- * Indexed Functors
  , FunctorWithIndex(..)
  -- ** Indexed Functor Combinators
  , imapped
  -- * Indexed Foldables
  , FoldableWithIndex(..)
  -- ** Indexed Foldable Combinators
  , ifolded
  , iany
  , iall
  , inone, none
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
  , withIndex
  , asIndex
  -- * Restricting by Index
  , indices
  , index
  -- * Indexed Traversables
  , TraversableWithIndex(..)
  -- * Indexed Traversable Combinators
  , itraversed
  , ifor
  , imapM
  , iforM
  , imapAccumR
  , imapAccumL
  -- * Indexed Folds with Reified Monoid
  , ifoldMapBy
  , ifoldMapByOf
  -- * Indexed Traversals with Reified Applicative
  , itraverseBy
  , itraverseByOf
  ) where

import Prelude ()

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Internal.Fold
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Prelude
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Reflection

import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Vector (Vector)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector

infixr 9 <.>, <., .>

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import qualified Data.Map as Map

-- | Compose an 'Indexed' function with a non-indexed function.
--
-- Mnemonically, the @<@ points to the indexing we want to preserve.
--
-- >>> let nestedMap = (fmap Map.fromList . Map.fromList) [(1, [(10, "one,ten"), (20, "one,twenty")]), (2, [(30, "two,thirty"), (40,"two,forty")])]
-- >>> nestedMap^..(itraversed<.itraversed).withIndex
-- [(1,"one,ten"),(1,"one,twenty"),(2,"two,thirty"),(2,"two,forty")]
(<.) :: Indexable i p => (Indexed i s t -> r) -> ((a -> b) -> s -> t) -> p a b -> r
(<.) f g h = f . Indexed $ g . indexed h
{-# INLINE (<.) #-}

-- | Compose a non-indexed function with an 'Indexed' function.
--
-- Mnemonically, the @>@ points to the indexing we want to preserve.
--
-- This is the same as @('.')@.
--
-- @f '.' g@ (and @f '.>' g@) gives you the index of @g@ unless @g@ is index-preserving, like a
-- 'Prism', 'Iso' or 'Equality', in which case it'll pass through the index of @f@.
--
-- >>> let nestedMap = (fmap Map.fromList . Map.fromList) [(1, [(10, "one,ten"), (20, "one,twenty")]), (2, [(30, "two,thirty"), (40,"two,forty")])]
-- >>> nestedMap^..(itraversed.>itraversed).withIndex
-- [(10,"one,ten"),(20,"one,twenty"),(30,"two,thirty"),(40,"two,forty")]
(.>) :: (st -> r) -> (kab -> st) -> kab -> r
(.>) = (.)
{-# INLINE (.>) #-}

-- | Use a value itself as its own index. This is essentially an indexed version of 'id'.
--
-- Note: When used to modify the value, this can break the index requirements assumed by 'indices' and similar,
-- so this is only properly an 'IndexedGetter', but it can be used as more.
--
-- @
-- 'selfIndex' :: 'IndexedGetter' a a b
-- @
selfIndex :: Indexable a p => p a fb -> a -> fb
selfIndex f a = indexed f a a
{-# INLINE selfIndex #-}

-- | Remap the index.
reindexed :: Indexable j p => (i -> j) -> (Indexed i a b -> r) -> p a b -> r
reindexed ij f g = f . Indexed $ indexed g . ij
{-# INLINE reindexed #-}

-- | Composition of 'Indexed' functions.
--
-- Mnemonically, the @\<@ and @\>@ points to the fact that we want to preserve the indices.
--
-- >>> let nestedMap = (fmap Map.fromList . Map.fromList) [(1, [(10, "one,ten"), (20, "one,twenty")]), (2, [(30, "two,thirty"), (40,"two,forty")])]
-- >>> nestedMap^..(itraversed<.>itraversed).withIndex
-- [((1,10),"one,ten"),((1,20),"one,twenty"),((2,30),"two,thirty"),((2,40),"two,forty")]
(<.>) :: Indexable (i, j) p => (Indexed i s t -> r) -> (Indexed j a b -> s -> t) -> p a b -> r
f <.> g = icompose (,) f g
{-# INLINE (<.>) #-}

-- | Composition of 'Indexed' functions with a user supplied function for combining indices.
icompose :: Indexable p c => (i -> j -> p) -> (Indexed i s t -> r) -> (Indexed j a b -> s -> t) -> c a b -> r
icompose ijk istr jabst cab = istr . Indexed $ \i -> jabst . Indexed $ \j -> indexed cab $ ijk i j
{-# INLINE icompose #-}

-------------------------------------------------------------------------------
-- Restricting by index
-------------------------------------------------------------------------------

-- | This allows you to filter an 'IndexedFold', 'IndexedGetter', 'IndexedTraversal' or 'IndexedLens' based on a predicate
-- on the indices.
--
-- >>> ["hello","the","world","!!!"]^..traversed.indices even
-- ["hello","world"]
--
-- >>> over (traversed.indices (>0)) Prelude.reverse $ ["He","was","stressed","o_O"]
-- ["He","saw","desserts","O_o"]
indices :: (Indexable i p, Applicative f) => (i -> Bool) -> Optical' p (Indexed i) f a a
indices p f = Indexed $ \i a -> if p i then indexed f i a else pure a
{-# INLINE indices #-}

-- | This allows you to filter an 'IndexedFold', 'IndexedGetter', 'IndexedTraversal' or 'IndexedLens' based on an index.
--
-- >>> ["hello","the","world","!!!"]^?traversed.index 2
-- Just "world"
index :: (Indexable i p, Eq i, Applicative f) => i -> Optical' p (Indexed i) f a a
index j f = Indexed $ \i a -> if j == i then indexed f i a else pure a
{-# INLINE index #-}


-------------------------------------------------------------------------------
-- FunctorWithIndex
-------------------------------------------------------------------------------

-- | The 'IndexedSetter' for a 'FunctorWithIndex'.
--
-- If you don't need access to the index, then 'mapped' is more flexible in what it accepts.
imapped :: FunctorWithIndex i f => IndexedSetter i (f a) (f b) a b
imapped = conjoined mapped (isets imap)
{-# INLINE imapped #-}

-------------------------------------------------------------------------------
-- FoldableWithIndex
-------------------------------------------------------------------------------

-- | The 'IndexedFold' of a 'FoldableWithIndex' container.
--
-- @'ifolded' '.' 'asIndex'@ is a fold over the keys of a 'FoldableWithIndex'.
--
-- >>> Data.Map.fromList [(2, "hello"), (1, "world")]^..ifolded.asIndex
-- [1,2]
ifolded :: FoldableWithIndex i f => IndexedFold i (f a) a
ifolded = conjoined folded $ \f -> phantom . getFolding . ifoldMap (\i -> Folding #. indexed f i)
{-# INLINE ifolded #-}

-------------------------------------------------------------------------------
-- TraversableWithIndex
-------------------------------------------------------------------------------

-- | The 'IndexedTraversal' of a 'TraversableWithIndex' container.
itraversed :: TraversableWithIndex i t => IndexedTraversal i (t a) (t b) a b
itraversed = conjoined traverse (itraverse . indexed)
{-# INLINE [0] itraversed #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

{-# RULES
"itraversed -> mapList"    itraversed = sets fmap        :: ASetter [a] [b] a b;
"itraversed -> imapList"   itraversed = isets imap       :: AnIndexedSetter Int [a] [b] a b;
"itraversed -> foldrList"  itraversed = foldring foldr   :: Getting (Endo r) [a] a;
"itraversed -> ifoldrList" itraversed = ifoldring ifoldr :: IndexedGetting Int (Endo r) [a] a;
 #-}

{-# RULES
"itraversed -> mapIntMap"    itraversed = sets IntMap.map               :: ASetter (IntMap a) (IntMap b) a b;
"itraversed -> imapIntMap"   itraversed = isets IntMap.mapWithKey       :: AnIndexedSetter Int (IntMap a) (IntMap b) a b;
"itraversed -> foldrIntMap"  itraversed = foldring IntMap.foldr         :: Getting (Endo r) (IntMap a) a;
"itraversed -> ifoldrIntMap" itraversed = ifoldring IntMap.foldrWithKey :: IndexedGetting Int (Endo r) (IntMap a) a;
 #-}

{-# RULES
"itraversed -> mapMap"    itraversed = sets Map.map               :: ASetter (Map k a) (Map k b) a b;
"itraversed -> imapMap"   itraversed = isets Map.mapWithKey       :: AnIndexedSetter k (Map k a) (Map k b) a b;
"itraversed -> foldrMap"  itraversed = foldring Map.foldr         :: Getting (Endo r) (Map k a) a;
"itraversed -> ifoldrMap" itraversed = ifoldring Map.foldrWithKey :: IndexedGetting k (Endo r) (Map k a) a;
 #-}

{-# RULES
"itraversed -> mapHashMap"    itraversed = sets HashMap.map               :: ASetter (HashMap k a) (HashMap k b) a b;
"itraversed -> imapHashMap"   itraversed = isets HashMap.mapWithKey       :: AnIndexedSetter k (HashMap k a) (HashMap k b) a b;
"itraversed -> foldrHashMap"  itraversed = foldring HashMap.foldr         :: Getting (Endo r) (HashMap k a) a;
"itraversed -> ifoldrHashMap" itraversed = ifoldring HashMap.foldrWithKey :: IndexedGetting k (Endo r) (HashMap k a) a;
 #-}

{-# RULES
"itraversed -> mapSeq"    itraversed = sets fmap                    :: ASetter (Seq a) (Seq b) a b;
"itraversed -> imapSeq"   itraversed = isets Seq.mapWithIndex       :: AnIndexedSetter Int (Seq a) (Seq b) a b;
"itraversed -> foldrSeq"  itraversed = foldring foldr               :: Getting (Endo r) (Seq a) a;
"itraversed -> ifoldrSeq" itraversed = ifoldring Seq.foldrWithIndex :: IndexedGetting Int (Endo r) (Seq a) a;
 #-}

{-# RULES
"itraversed -> mapVector"    itraversed = sets Vector.map         :: ASetter (Vector a) (Vector b) a b;
"itraversed -> imapVector"   itraversed = isets Vector.imap       :: AnIndexedSetter Int (Vector a) (Vector b) a b;
"itraversed -> foldrVector"  itraversed = foldring Vector.foldr   :: Getting (Endo r) (Vector a) a;
"itraversed -> ifoldrVector" itraversed = ifoldring Vector.ifoldr :: IndexedGetting Int (Endo r) (Vector a) a;
 #-}

-------------------------------------------------------------------------------
-- Indexed Folds with Reified Monoid
-------------------------------------------------------------------------------

ifoldMapBy :: FoldableWithIndex i t => (r -> r -> r) -> r -> (i -> a -> r) -> t a -> r
ifoldMapBy f z g = reifyMonoid f z (ifoldMap (\i a -> ReflectedMonoid (g i a)))

ifoldMapByOf :: IndexedFold i t a -> (r -> r -> r) -> r -> (i -> a -> r) -> t -> r
ifoldMapByOf l f z g = reifyMonoid f z (ifoldMapOf l (\i a -> ReflectedMonoid (g i a)))

itraverseBy :: TraversableWithIndex i t => (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> (i -> a -> f b) -> t a -> f (t b)
itraverseBy pur app f = reifyApplicative pur app (itraverse (\i a -> ReflectedApplicative (f i a)))

itraverseByOf :: IndexedTraversal i s t a b -> (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> (i -> a -> f b) -> s -> f t
itraverseByOf l pur app f = reifyApplicative pur app (itraverseOf l (\i a -> ReflectedApplicative (f i a)))
