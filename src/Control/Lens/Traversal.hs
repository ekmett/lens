{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds #-}

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Traversal
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Traversal' s t a b@ is a generalization of 'traverse' from
-- 'Traversable'. It allows you to 'traverse' over a structure and change out
-- its contents with monadic or 'Applicative' side-effects. Starting from
--
-- @
-- 'traverse' :: ('Traversable' t, 'Applicative' f) => (a -> f b) -> t a -> f (t b)
-- @
--
-- we monomorphize the contents and result to obtain
--
-- @
-- type 'Traversal' s t a b = forall f. 'Applicative' f => (a -> f b) -> s -> f t
-- @
--
-- A 'Traversal' can be used as a 'Fold'.
-- Any 'Traversal' can be used for 'Control.Lens.Getter.Getting' like a 'Fold',
-- because given a 'Data.Monoid.Monoid' @m@, we have an 'Applicative' for
-- @('Const' m)@. Everything you know how to do with a 'Traversable' container,
-- you can with a 'Traversal', and here we provide combinators that generalize
-- the usual 'Traversable' operations.
----------------------------------------------------------------------------
module Control.Lens.Traversal
  (
  -- * Traversals
    Traversal, Traversal'
  , Traversal1, Traversal1'
  , IndexedTraversal, IndexedTraversal'
  , IndexedTraversal1, IndexedTraversal1'
  , ATraversal, ATraversal'
  , ATraversal1, ATraversal1'
  , AnIndexedTraversal, AnIndexedTraversal'
  , AnIndexedTraversal1, AnIndexedTraversal1'
  , Traversing, Traversing'
  , Traversing1, Traversing1'

  -- * Traversing and Lensing
  , traverseOf, forOf, sequenceAOf
  , mapMOf, forMOf, sequenceOf
  , transposeOf
  , mapAccumLOf, mapAccumROf
  , scanr1Of, scanl1Of
  , failover, ifailover

  -- * Monomorphic Traversals
  , cloneTraversal
  , cloneIndexPreservingTraversal
  , cloneIndexedTraversal
  , cloneTraversal1
  , cloneIndexPreservingTraversal1
  , cloneIndexedTraversal1

  -- * Parts and Holes
  , partsOf, partsOf'
  , unsafePartsOf, unsafePartsOf'
  , holesOf, holes1Of
  , singular, unsafeSingular

  -- * Common Traversals
  , Traversable(traverse)
  , Traversable1(traverse1)
  , both, both1
  , beside
  , taking
  , dropping
  , failing
  , deepOf

  -- * Indexed Traversals

  -- ** Common
  , ignored
  , TraverseMin(..)
  , TraverseMax(..)
  , traversed
  , traversed1
  , traversed64
  , elementOf
  , element
  , elementsOf
  , elements

  -- ** Combinators
  , ipartsOf
  , ipartsOf'
  , iunsafePartsOf
  , iunsafePartsOf'
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Reflection
  , traverseBy
  , traverseByOf
  , sequenceBy
  , sequenceByOf

  -- * Implementation Details
  , Bazaar(..), Bazaar'
  , Bazaar1(..), Bazaar1'
  , loci
  , iloci

  -- * Fusion
  , confusing
  ) where

import Control.Applicative as Applicative
import Control.Applicative.Backwards
import Control.Category
import Control.Comonad
import Control.Lens.Fold
import Control.Lens.Getter (Getting, IndexedGetting, getting)
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Fold
import Control.Lens.Lens
import Control.Lens.Setter (ASetter, AnIndexedSetter, isets, sets)
import Control.Lens.Type
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Bitraversable
import Data.CallStack
import Data.Functor.Apply
import Data.Functor.Compose
import Data.Functor.Day.Curried
import Data.Functor.Yoneda
import Data.Int
import Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Sequence (Seq, mapWithIndex)
import Data.Vector as Vector (Vector, imap)
import Data.Monoid (Any (..), Endo (..))
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Data.Reflection
import Data.Semigroup.Traversable
import Data.Semigroup.Bitraversable
import Data.Traversable
import Data.Tuple (swap)
import GHC.Magic (inline)
import Prelude hiding ((.),id)

#if !(MIN_VERSION_base(4,8,0))
import Data.Foldable (Foldable)
import Data.Monoid (Monoid (..))
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup (..))
#endif

-- $setup
-- >>> :set -XNoOverloadedStrings -XFlexibleContexts
-- >>> import Data.Char (toUpper)
-- >>> import Control.Lens
-- >>> import Control.DeepSeq (NFData (..), force)
-- >>> import Control.Exception (evaluate,try,ErrorCall(..))
-- >>> import Data.Maybe (fromMaybe)
-- >>> import Debug.SimpleReflect.Vars
-- >>> import Data.Void
-- >>> import Data.List (sort)
-- >>> import System.Timeout (timeout)
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> let timingOut :: NFData a => a -> IO a; timingOut = fmap (fromMaybe (error "timeout")) . timeout (5*10^6) . evaluate . force

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects a 'Traversal'.
type ATraversal s t a b = LensLike (Bazaar (->) a b) s t a b

-- | @
-- type 'ATraversal'' = 'Simple' 'ATraversal'
-- @
type ATraversal' s a = ATraversal s s a a


-- | When you see this as an argument to a function, it expects a 'Traversal1'.
type ATraversal1 s t a b = LensLike (Bazaar1 (->) a b) s t a b

-- | @
-- type 'ATraversal1'' = 'Simple' 'ATraversal1'
-- @
type ATraversal1' s a = ATraversal1 s s a a

-- | When you see this as an argument to a function, it expects an 'IndexedTraversal'.
type AnIndexedTraversal i s t a b = Over (Indexed i) (Bazaar (Indexed i) a b) s t a b

-- | When you see this as an argument to a function, it expects an 'IndexedTraversal1'.
type AnIndexedTraversal1 i s t a b = Over (Indexed i) (Bazaar1 (Indexed i) a b) s t a b

-- | @
-- type 'AnIndexedTraversal'' = 'Simple' ('AnIndexedTraversal' i)
-- @
type AnIndexedTraversal' i s a = AnIndexedTraversal i s s a a

-- | @
-- type 'AnIndexedTraversal1'' = 'Simple' ('AnIndexedTraversal1' i)
-- @
type AnIndexedTraversal1' i s a = AnIndexedTraversal1 i s s a a


-- | When you see this as an argument to a function, it expects
--
--  * to be indexed if @p@ is an instance of 'Indexed' i,
--
--  * to be unindexed if @p@ is @(->)@,
--
--  * a 'Traversal' if @f@ is 'Applicative',
--
--  * a 'Getter' if @f@ is only a 'Functor' and 'Data.Functor.Contravariant.Contravariant',
--
--  * a 'Lens' if @f@ is only a 'Functor',
--
--  * a 'Fold' if @f@ is 'Applicative' and 'Data.Functor.Contravariant.Contravariant'.
type Traversing p f s t a b = Over p (BazaarT p f a b) s t a b

type Traversing1 p f s t a b = Over p (BazaarT1 p f a b) s t a b

-- | @
-- type 'Traversing'' f = 'Simple' ('Traversing' f)
-- @
type Traversing' p f s a = Traversing p f s s a a
type Traversing1' p f s a = Traversing1 p f s s a a

--------------------------
-- Traversal Combinators
--------------------------

-- | Map each element of a structure targeted by a 'Lens' or 'Traversal',
-- evaluate these actions from left to right, and collect the results.
--
-- This function is only provided for consistency, 'id' is strictly more general.
--
-- >>> traverseOf each print (1,2,3)
-- 1
-- 2
-- 3
-- ((),(),())
--
-- @
-- 'traverseOf' ≡ 'id'
-- 'itraverseOf' l ≡ 'traverseOf' l '.' 'Indexed'
-- 'itraverseOf' 'itraversed' ≡ 'itraverse'
-- @
--
--
-- This yields the obvious law:
--
-- @
-- 'traverse' ≡ 'traverseOf' 'traverse'
-- @
--
-- @
-- 'traverseOf' :: 'Functor' f     => 'Iso' s t a b        -> (a -> f b) -> s -> f t
-- 'traverseOf' :: 'Functor' f     => 'Lens' s t a b       -> (a -> f b) -> s -> f t
-- 'traverseOf' :: 'Apply' f       => 'Traversal1' s t a b -> (a -> f b) -> s -> f t
-- 'traverseOf' :: 'Applicative' f => 'Traversal' s t a b  -> (a -> f b) -> s -> f t
-- @
traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
traverseOf = id
{-# INLINE traverseOf #-}

-- | A version of 'traverseOf' with the arguments flipped, such that:
--
-- >>> forOf each (1,2,3) print
-- 1
-- 2
-- 3
-- ((),(),())
--
-- This function is only provided for consistency, 'flip' is strictly more general.
--
-- @
-- 'forOf' ≡ 'flip'
-- 'forOf' ≡ 'flip' . 'traverseOf'
-- @
--
-- @
-- 'for' ≡ 'forOf' 'traverse'
-- 'Control.Lens.Indexed.ifor' l s ≡ 'for' l s '.' 'Indexed'
-- @
--
-- @
-- 'forOf' :: 'Functor' f => 'Iso' s t a b -> s -> (a -> f b) -> f t
-- 'forOf' :: 'Functor' f => 'Lens' s t a b -> s -> (a -> f b) -> f t
-- 'forOf' :: 'Applicative' f => 'Traversal' s t a b -> s -> (a -> f b) -> f t
-- @
forOf :: LensLike f s t a b -> s -> (a -> f b) -> f t
forOf = flip
{-# INLINE forOf #-}

-- | Evaluate each action in the structure from left to right, and collect
-- the results.
--
-- >>> sequenceAOf both ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- @
-- 'sequenceA' ≡ 'sequenceAOf' 'traverse' ≡ 'traverse' 'id'
-- 'sequenceAOf' l ≡ 'traverseOf' l 'id' ≡ l 'id'
-- @
--
-- @
-- 'sequenceAOf' :: 'Functor' f => 'Iso' s t (f b) b       -> s -> f t
-- 'sequenceAOf' :: 'Functor' f => 'Lens' s t (f b) b      -> s -> f t
-- 'sequenceAOf' :: 'Applicative' f => 'Traversal' s t (f b) b -> s -> f t
-- @
sequenceAOf :: LensLike f s t (f b) b -> s -> f t
sequenceAOf l = l id
{-# INLINE sequenceAOf #-}

-- | Map each element of a structure targeted by a 'Lens' to a monadic action,
-- evaluate these actions from left to right, and collect the results.
--
-- >>> mapMOf both (\x -> [x, x + 1]) (1,3)
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- @
-- 'mapM' ≡ 'mapMOf' 'traverse'
-- 'imapMOf' l ≡ 'forM' l '.' 'Indexed'
-- @
--
-- @
-- 'mapMOf' :: 'Monad' m => 'Iso' s t a b       -> (a -> m b) -> s -> m t
-- 'mapMOf' :: 'Monad' m => 'Lens' s t a b      -> (a -> m b) -> s -> m t
-- 'mapMOf' :: 'Monad' m => 'Traversal' s t a b -> (a -> m b) -> s -> m t
-- @
mapMOf :: LensLike (WrappedMonad m) s t a b -> (a -> m b) -> s -> m t
mapMOf l cmd = unwrapMonad #. l (WrapMonad #. cmd)
{-# INLINE mapMOf #-}

-- | 'forMOf' is a flipped version of 'mapMOf', consistent with the definition of 'forM'.
--
-- >>> forMOf both (1,3) $ \x -> [x, x + 1]
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- @
-- 'forM' ≡ 'forMOf' 'traverse'
-- 'forMOf' l ≡ 'flip' ('mapMOf' l)
-- 'iforMOf' l s ≡ 'forM' l s '.' 'Indexed'
-- @
--
-- @
-- 'forMOf' :: 'Monad' m => 'Iso' s t a b       -> s -> (a -> m b) -> m t
-- 'forMOf' :: 'Monad' m => 'Lens' s t a b      -> s -> (a -> m b) -> m t
-- 'forMOf' :: 'Monad' m => 'Traversal' s t a b -> s -> (a -> m b) -> m t
-- @
forMOf :: LensLike (WrappedMonad m) s t a b -> s -> (a -> m b) -> m t
forMOf l a cmd = unwrapMonad (l (WrapMonad #. cmd) a)
{-# INLINE forMOf #-}

-- | Sequence the (monadic) effects targeted by a 'Lens' in a container from left to right.
--
-- >>> sequenceOf each ([1,2],[3,4],[5,6])
-- [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]
--
-- @
-- 'sequence' ≡ 'sequenceOf' 'traverse'
-- 'sequenceOf' l ≡ 'mapMOf' l 'id'
-- 'sequenceOf' l ≡ 'unwrapMonad' '.' l 'WrapMonad'
-- @
--
-- @
-- 'sequenceOf' :: 'Monad' m => 'Iso' s t (m b) b       -> s -> m t
-- 'sequenceOf' :: 'Monad' m => 'Lens' s t (m b) b      -> s -> m t
-- 'sequenceOf' :: 'Monad' m => 'Traversal' s t (m b) b -> s -> m t
-- @
sequenceOf :: LensLike (WrappedMonad m) s t (m b) b -> s -> m t
sequenceOf l = unwrapMonad #. l WrapMonad
{-# INLINE sequenceOf #-}

-- | This generalizes 'Data.List.transpose' to an arbitrary 'Traversal'.
--
-- Note: 'Data.List.transpose' handles ragged inputs more intelligently, but for non-ragged inputs:
--
-- >>> transposeOf traverse [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
--
-- @
-- 'Data.List.transpose' ≡ 'transposeOf' 'traverse'
-- @
--
-- Since every 'Lens' is a 'Traversal', we can use this as a form of
-- monadic strength as well:
--
-- @
-- 'transposeOf' 'Control.Lens.Tuple._2' :: (b, [a]) -> [(b, a)]
-- @
transposeOf :: LensLike ZipList s t [a] a -> s -> [t]
transposeOf l = getZipList #. l ZipList
{-# INLINE transposeOf #-}

-- | This generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal'.
--
-- @
-- 'mapAccumR' ≡ 'mapAccumROf' 'traverse'
-- @
--
-- 'mapAccumROf' accumulates 'State' from right to left.
--
-- @
-- 'mapAccumROf' :: 'Iso' s t a b       -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumROf' :: 'Lens' s t a b      -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumROf' :: 'Traversal' s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
--
-- @
-- 'mapAccumROf' :: 'LensLike' ('Backwards' ('State' acc)) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
mapAccumROf :: LensLike (Backwards (State acc)) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumROf = mapAccumLOf . backwards
{-# INLINE mapAccumROf #-}

-- | This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'.
--
-- @
-- 'mapAccumL' ≡ 'mapAccumLOf' 'traverse'
-- @
--
-- 'mapAccumLOf' accumulates 'State' from left to right.
--
-- @
-- 'mapAccumLOf' :: 'Iso' s t a b       -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' :: 'Lens' s t a b      -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' :: 'Traversal' s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
--
-- @
-- 'mapAccumLOf' :: 'LensLike' ('State' acc) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' l f acc0 s = 'swap' ('runState' (l (\a -> 'state' (\acc -> 'swap' (f acc a))) s) acc0)
-- @
--
mapAccumLOf :: LensLike (State acc) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumLOf l f acc0 s = swap (runState (l g s) acc0) where
   g a = state $ \acc -> swap (f acc a)
-- This would be much cleaner if the argument order for the function was swapped.
{-# INLINE mapAccumLOf #-}

-- | This permits the use of 'scanr1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @
-- 'scanr1' ≡ 'scanr1Of' 'traverse'
-- @
--
-- @
-- 'scanr1Of' :: 'Iso' s t a a       -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Lens' s t a a      -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Traversal' s t a a -> (a -> a -> a) -> s -> t
-- @
scanr1Of :: LensLike (Backwards (State (Maybe a))) s t a a -> (a -> a -> a) -> s -> t
scanr1Of l f = snd . mapAccumROf l step Nothing where
  step Nothing a  = (Just a, a)
  step (Just s) a = (Just r, r) where r = f a s
{-# INLINE scanr1Of #-}

-- | This permits the use of 'scanl1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @
-- 'scanl1' ≡ 'scanl1Of' 'traverse'
-- @
--
-- @
-- 'scanl1Of' :: 'Iso' s t a a       -> (a -> a -> a) -> s -> t
-- 'scanl1Of' :: 'Lens' s t a a      -> (a -> a -> a) -> s -> t
-- 'scanl1Of' :: 'Traversal' s t a a -> (a -> a -> a) -> s -> t
-- @
scanl1Of :: LensLike (State (Maybe a)) s t a a -> (a -> a -> a) -> s -> t
scanl1Of l f = snd . mapAccumLOf l step Nothing where
  step Nothing a  = (Just a, a)
  step (Just s) a = (Just r, r) where r = f s a
{-# INLINE scanl1Of #-}

-- | This 'Traversal' allows you to 'traverse' the individual stores in a 'Bazaar'.
loci :: Traversal (Bazaar (->) a c s) (Bazaar (->) b c s) a b
loci f w = getCompose (runBazaar w (Compose #. fmap sell . f))
{-# INLINE loci #-}

-- | This 'IndexedTraversal' allows you to 'traverse' the individual stores in
-- a 'Bazaar' with access to their indices.
iloci :: IndexedTraversal i (Bazaar (Indexed i) a c s) (Bazaar (Indexed i) b c s) a b
iloci f w = getCompose (runBazaar w (Compose #. Indexed (\i -> fmap (indexed sell i) . indexed f i)))
{-# INLINE iloci #-}

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

-- | 'partsOf' turns a 'Traversal' into a 'Lens' that resembles an early version of the 'Data.Data.Lens.uniplate' (or 'Data.Data.Lens.biplate') type.
--
-- /Note:/ You should really try to maintain the invariant of the number of children in the list.
--
-- >>> (a,b,c) & partsOf each .~ [x,y,z]
-- (x,y,z)
--
-- Any extras will be lost. If you do not supply enough, then the remainder will come from the original structure.
--
-- >>> (a,b,c) & partsOf each .~ [w,x,y,z]
-- (w,x,y)
--
-- >>> (a,b,c) & partsOf each .~ [x,y]
-- (x,y,c)
--
-- >>> ('b', 'a', 'd', 'c') & partsOf each %~ sort
-- ('a','b','c','d')
--
-- So technically, this is only a 'Lens' if you do not change the number of results it returns.
--
-- When applied to a 'Fold' the result is merely a 'Getter'.
--
-- @
-- 'partsOf' :: 'Iso'' s a       -> 'Lens'' s [a]
-- 'partsOf' :: 'Lens'' s a      -> 'Lens'' s [a]
-- 'partsOf' :: 'Traversal'' s a -> 'Lens'' s [a]
-- 'partsOf' :: 'Fold' s a       -> 'Getter' s [a]
-- 'partsOf' :: 'Getter' s a     -> 'Getter' s [a]
-- @
partsOf :: Functor f => Traversing (->) f s t a a -> LensLike f s t [a] [a]
partsOf l f s = outs b <$> f (ins b) where b = l sell s
{-# INLINE partsOf #-}

-- | An indexed version of 'partsOf' that receives the entire list of indices as its index.
ipartsOf :: forall i p f s t a. (Indexable [i] p, Functor f) => Traversing (Indexed i) f s t a a -> Over p f s t [a] [a]
ipartsOf l = conjoined
  (\f s -> let b = inline l sell s                            in outs b <$> f (wins b))
  (\f s -> let b = inline l sell s; (is, as) = unzip (pins b) in outs b <$> indexed f (is :: [i]) as)
{-# INLINE ipartsOf #-}

-- | A type-restricted version of 'partsOf' that can only be used with a 'Traversal'.
partsOf' :: ATraversal s t a a -> Lens s t [a] [a]
partsOf' l f s = outs b <$> f (ins b) where b = l sell s
{-# INLINE partsOf' #-}

-- | A type-restricted version of 'ipartsOf' that can only be used with an 'IndexedTraversal'.
ipartsOf' :: forall i p f s t a. (Indexable [i] p, Functor f) => Over (Indexed i) (Bazaar' (Indexed i) a) s t a a -> Over p f s t [a] [a]
ipartsOf' l = conjoined
  (\f s -> let b = inline l sell s                            in outs b <$> f (wins b))
  (\f s -> let b = inline l sell s; (is, as) = unzip (pins b) in outs b <$> indexed f (is :: [i]) as)
{-# INLINE ipartsOf' #-}

-- | 'unsafePartsOf' turns a 'Traversal' into a 'Data.Data.Lens.uniplate' (or 'Data.Data.Lens.biplate') family.
--
-- If you do not need the types of @s@ and @t@ to be different, it is recommended that
-- you use 'partsOf'.
--
-- It is generally safer to traverse with the 'Bazaar' rather than use this
-- combinator. However, it is sometimes convenient.
--
-- This is unsafe because if you don't supply at least as many @b@'s as you were
-- given @a@'s, then the reconstruction of @t@ /will/ result in an error!
--
-- When applied to a 'Fold' the result is merely a 'Getter' (and becomes safe).
--
-- @
-- 'unsafePartsOf' :: 'Iso' s t a b       -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Lens' s t a b      -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Traversal' s t a b -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Fold' s a          -> 'Getter' s [a]
-- 'unsafePartsOf' :: 'Getter' s a        -> 'Getter' s [a]
-- @
unsafePartsOf :: Functor f => Traversing (->) f s t a b -> LensLike f s t [a] [b]
unsafePartsOf l f s = unsafeOuts b <$> f (ins b) where b = l sell s
{-# INLINE unsafePartsOf #-}

-- | An indexed version of 'unsafePartsOf' that receives the entire list of indices as its index.
iunsafePartsOf :: forall i p f s t a b. (Indexable [i] p, Functor f) => Traversing (Indexed i) f s t a b -> Over p f s t [a] [b]
iunsafePartsOf l = conjoined
  (\f s -> let b = inline l sell s                           in unsafeOuts b <$> f (wins b))
  (\f s -> let b = inline l sell s; (is,as) = unzip (pins b) in unsafeOuts b <$> indexed f (is :: [i]) as)
{-# INLINE iunsafePartsOf #-}

unsafePartsOf' :: ATraversal s t a b -> Lens s t [a] [b]
unsafePartsOf' l f s = unsafeOuts b <$> f (ins b) where b = l sell s
{-# INLINE unsafePartsOf' #-}

iunsafePartsOf' :: forall i s t a b. Over (Indexed i) (Bazaar (Indexed i) a b) s t a b -> IndexedLens [i] s t [a] [b]
iunsafePartsOf' l = conjoined
  (\f s -> let b = inline l sell s                            in unsafeOuts b <$> f (wins b))
  (\f s -> let b = inline l sell s; (is, as) = unzip (pins b) in unsafeOuts b <$> indexed f (is :: [i]) as)
{-# INLINE iunsafePartsOf' #-}


-- | This converts a 'Traversal' that you \"know\" will target one or more elements to a 'Lens'. It can
-- also be used to transform a non-empty 'Fold' into a 'Getter'.
--
-- The resulting 'Lens' or 'Getter' will be partial if the supplied 'Traversal' returns
-- no results.
--
-- >>> [1,2,3] ^. singular _head
-- 1
--
-- >>> Left (ErrorCall "singular: empty traversal") <- try (evaluate ([] ^. singular _head)) :: IO (Either ErrorCall ())
--
-- >>> Left 4 ^. singular _Left
-- 4
--
-- >>> [1..10] ^. singular (ix 7)
-- 8
--
-- >>> [] & singular traverse .~ 0
-- []
--
-- @
-- 'singular' :: 'Traversal' s t a a          -> 'Lens' s t a a
-- 'singular' :: 'Fold' s a                   -> 'Getter' s a
-- 'singular' :: 'IndexedTraversal' i s t a a -> 'IndexedLens' i s t a a
-- 'singular' :: 'IndexedFold' i s a          -> 'IndexedGetter' i s a
-- @
singular :: (HasCallStack, Conjoined p, Functor f)
         => Traversing p f s t a a
         -> Over p f s t a a
singular l = conjoined
  (\afb s -> let b = l sell s in case ins b of
    (w:ws) -> unsafeOuts b . (:ws) <$> afb w
    []     -> unsafeOuts b . return <$> afb (error "singular: empty traversal"))
  (\pafb s -> let b = l sell s in case pins b of
    (w:ws) -> unsafeOuts b . (:Prelude.map extract ws) <$> cosieve pafb w
    []     -> unsafeOuts b . return                    <$> cosieve pafb (error "singular: empty traversal"))
{-# INLINE singular #-}

-- | This converts a 'Traversal' that you \"know\" will target only one element to a 'Lens'. It can also be
-- used to transform a 'Fold' into a 'Getter'.
--
-- The resulting 'Lens' or 'Getter' will be partial if the 'Traversal' targets nothing
-- or more than one element.
--
-- >>> Left (ErrorCall "unsafeSingular: empty traversal") <- try (evaluate ([] & unsafeSingular traverse .~ 0)) :: IO (Either ErrorCall [Integer])
--
-- @
-- 'unsafeSingular' :: 'Traversal' s t a b          -> 'Lens' s t a b
-- 'unsafeSingular' :: 'Fold' s a                   -> 'Getter' s a
-- 'unsafeSingular' :: 'IndexedTraversal' i s t a b -> 'IndexedLens' i s t a b
-- 'unsafeSingular' :: 'IndexedFold' i s a          -> 'IndexedGetter' i s a
-- @
unsafeSingular :: (HasCallStack, Conjoined p, Functor f)
               => Traversing p f s t a b
               -> Over p f s t a b
unsafeSingular l = conjoined
  (\afb s -> let b = inline l sell s in case ins b of
    [w] -> unsafeOuts b . return <$> afb w
    []  -> error "unsafeSingular: empty traversal"
    _   -> error "unsafeSingular: traversing multiple results")
  (\pafb s -> let b = inline l sell s in case pins b of
    [w] -> unsafeOuts b . return <$> cosieve pafb w
    []  -> error "unsafeSingular: empty traversal"
    _   -> error "unsafeSingular: traversing multiple results")
{-# INLINE unsafeSingular #-}

------------------------------------------------------------------------------
-- Internal functions used by 'partsOf', etc.
------------------------------------------------------------------------------

ins :: Bizarre (->) w => w a b t -> [a]
ins = toListOf (getting bazaar)
{-# INLINE ins #-}

wins :: (Bizarre p w, Corepresentable p, Comonad (Corep p)) => w a b t -> [a]
wins = getConst #. bazaar (cotabulate $ \ra -> Const [extract ra])
{-# INLINE wins #-}

pins :: (Bizarre p w, Corepresentable p) => w a b t -> [Corep p a]
pins = getConst #. bazaar (cotabulate $ \ra -> Const [ra])
{-# INLINE pins #-}

parr :: (Profunctor p, Category p) => (a -> b) -> p a b
parr f = lmap f id
{-# INLINE parr #-}

outs :: (Bizarre p w, Category p) => w a a t -> [a] -> t
outs = evalState `rmap` bazaar (parr (state . unconsWithDefault))
{-# INLINE outs #-}

unsafeOuts :: (Bizarre p w, Corepresentable p) => w a b t -> [b] -> t
unsafeOuts = evalState `rmap` bazaar (cotabulate (\_ -> state (unconsWithDefault fakeVal)))
  where fakeVal = error "unsafePartsOf': not enough elements were supplied"
{-# INLINE unsafeOuts #-}

unconsWithDefault :: a -> [a] -> (a,[a])
unconsWithDefault d []     = (d,[])
unconsWithDefault _ (x:xs) = (x,xs)
{-# INLINE unconsWithDefault #-}


-------------------------------------------------------------------------------
-- Holes
-------------------------------------------------------------------------------

-- | The one-level version of 'Control.Lens.Plated.contextsOf'. This extracts a
-- list of the immediate children according to a given 'Traversal' as editable
-- contexts.
--
-- Given a context you can use 'Control.Comonad.Store.Class.pos' to see the
-- values, 'Control.Comonad.Store.Class.peek' at what the structure would be
-- like with an edited result, or simply 'extract' the original structure.
--
-- @
-- propChildren l x = 'toListOf' l x '==' 'map' 'Control.Comonad.Store.Class.pos' ('holesOf' l x)
-- propId l x = 'all' ('==' x) ['extract' w | w <- 'holesOf' l x]
-- @
--
-- @
-- 'holesOf' :: 'Iso'' s a                -> s -> ['Pretext'' (->) a s]
-- 'holesOf' :: 'Lens'' s a               -> s -> ['Pretext'' (->) a s]
-- 'holesOf' :: 'Traversal'' s a          -> s -> ['Pretext'' (->) a s]
-- 'holesOf' :: 'IndexedLens'' i s a      -> s -> ['Pretext'' ('Indexed' i) a s]
-- 'holesOf' :: 'IndexedTraversal'' i s a -> s -> ['Pretext'' ('Indexed' i) a s]
-- @
holesOf :: Conjoined p
        => Over p (Bazaar p a a) s t a a -> s -> [Pretext p a a t]
holesOf f xs = flip appEndo [] . fst $
  runHoles (runBazaar (f sell xs) (cotabulate holeInOne)) id
{-# INLINE holesOf #-}

holeInOne :: (Corepresentable p, Comonad (Corep p))
          => Corep p a -> Holes t (Endo [Pretext p a a t]) a
holeInOne x = Holes $ \xt ->
    ( Endo (fmap xt (cosieve sell x) :)
    , extract x)
{-# INLINABLE holeInOne #-}

-- | The non-empty version of 'holesOf'.
-- This extract a non-empty list of immediate children accroding to a given
-- 'Traversal1' as editable contexts.
--
-- >>> let head1 f s = runPretext (NonEmpty.head $ holes1Of traversed1 s) f
-- >>> ('a' :| "bc") ^. head1
-- 'a'
--
-- >>> ('a' :| "bc") & head1 %~ toUpper
-- 'A' :| "bc"
--
-- @
-- 'holes1Of' :: 'Iso'' s a                 -> s -> 'NonEmpty' ('Pretext'' (->) a s)
-- 'holes1Of' :: 'Lens'' s a                -> s -> 'NonEmpty' ('Pretext'' (->) a s)
-- 'holes1Of' :: 'Traversal1'' s a          -> s -> 'NonEmpty' ('Pretext'' (->) a s)
-- 'holes1Of' :: 'IndexedLens'' i s a       -> s -> 'NonEmpty' ('Pretext'' ('Indexed' i) a s)
-- 'holes1Of' :: 'IndexedTraversal1'' i s a -> s -> 'NonEmpty' ('Pretext'' ('Indexed' i) a s)
-- @
holes1Of :: Conjoined p
         => Over p (Bazaar1 p a a) s t a a -> s -> NonEmpty (Pretext p a a t)
holes1Of f xs = flip getNonEmptyDList [] . fst $
  runHoles (runBazaar1 (f sell xs) (cotabulate holeInOne1)) id
{-# INLINE holes1Of #-}

holeInOne1 :: forall p a t. (Corepresentable p, Category p)
          => Corep p a -> Holes t (NonEmptyDList (Pretext p a a t)) a
holeInOne1 x = Holes $ \xt ->
    ( NonEmptyDList (fmap xt (cosieve sell x) :|)
    , cosieve (id :: p a a) x)

-- We are very careful to share as much structure as possible among
-- the results (in the common case where the traversal allows for such).
-- Note in particular the recursive knot in the implementation of <*>
-- for Holes. This sharing magic was inspired by Noah "Rampion" Easterly's
-- implementation of a related holes function: see
-- https://stackoverflow.com/a/49001904/1477667. The Holes type is
-- inspired by Roman Cheplyaka's answer to that same question.

newtype Holes t m x = Holes { runHoles :: (x -> t) -> (m, x) }

instance Functor (Holes t m) where
  fmap f xs = Holes $ \xt ->
    let
      (qf, qv) = runHoles xs (xt . f)
    in (qf, f qv)

instance Semigroup m => Apply (Holes t m) where
  fs <.> xs = Holes $ \xt ->
    let
     (pf, pv) = runHoles fs (xt . ($ qv))
     (qf, qv) = runHoles xs (xt . pv)
    in (pf <> qf, pv qv)

instance Monoid m => Applicative (Holes t m) where
  pure x = Holes $ \_ -> (mempty, x)

  fs <*> xs = Holes $ \xt ->
    let
     (pf, pv) = runHoles fs (xt . ($ qv))
     (qf, qv) = runHoles xs (xt . pv)
    in (pf `mappend` qf, pv qv)

#if MIN_VERSION_base(4,10,0)
  liftA2 f xs ys = Holes $ \xt ->
    let
      (pf, pv) = runHoles xs (xt . flip f qv)
      (qf, qv) = runHoles ys (xt . f pv)
    in (pf `mappend` qf, f pv qv)
#endif


------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | Traverse both parts of a 'Bitraversable' container with matching types.
--
-- Usually that type will be a pair. Use 'Control.Lens.Each.each' to traverse
-- the elements of arbitrary homogeneous tuples.
--
-- >>> (1,2) & both *~ 10
-- (10,20)
--
-- >>> over both length ("hello","world")
-- (5,5)
--
-- >>> ("hello","world")^.both
-- "helloworld"
--
-- @
-- 'both' :: 'Traversal' (a, a)       (b, b)       a b
-- 'both' :: 'Traversal' ('Either' a a) ('Either' b b) a b
-- @
both :: Bitraversable r => Traversal (r a a) (r b b) a b
both f = bitraverse f f
{-# INLINE both #-}

-- | Traverse both parts of a 'Bitraversable1' container with matching types.
--
-- Usually that type will be a pair.
--
-- @
-- 'both1' :: 'Traversal1' (a, a)       (b, b)       a b
-- 'both1' :: 'Traversal1' ('Either' a a) ('Either' b b) a b
-- @
both1 :: Bitraversable1 r => Traversal1 (r a a) (r b b) a b
both1 f = bitraverse1 f f
{-# INLINE both1 #-}

-- | Apply a different 'Traversal' or 'Fold' to each side of a 'Bitraversable' container.
--
-- @
-- 'beside' :: 'Traversal' s t a b                -> 'Traversal' s' t' a b                -> 'Traversal' (r s s') (r t t') a b
-- 'beside' :: 'IndexedTraversal' i s t a b       -> 'IndexedTraversal' i s' t' a b       -> 'IndexedTraversal' i (r s s') (r t t') a b
-- 'beside' :: 'IndexPreservingTraversal' s t a b -> 'IndexPreservingTraversal' s' t' a b -> 'IndexPreservingTraversal' (r s s') (r t t') a b
-- @
--
-- @
-- 'beside' :: 'Traversal' s t a b                -> 'Traversal' s' t' a b                -> 'Traversal' (s,s') (t,t') a b
-- 'beside' :: 'Lens' s t a b                     -> 'Lens' s' t' a b                     -> 'Traversal' (s,s') (t,t') a b
-- 'beside' :: 'Fold' s a                         -> 'Fold' s' a                          -> 'Fold' (s,s') a
-- 'beside' :: 'Getter' s a                       -> 'Getter' s' a                        -> 'Fold' (s,s') a
-- @
--
-- @
-- 'beside' :: 'IndexedTraversal' i s t a b       -> 'IndexedTraversal' i s' t' a b       -> 'IndexedTraversal' i (s,s') (t,t') a b
-- 'beside' :: 'IndexedLens' i s t a b            -> 'IndexedLens' i s' t' a b            -> 'IndexedTraversal' i (s,s') (t,t') a b
-- 'beside' :: 'IndexedFold' i s a                -> 'IndexedFold' i s' a                 -> 'IndexedFold' i (s,s') a
-- 'beside' :: 'IndexedGetter' i s a              -> 'IndexedGetter' i s' a               -> 'IndexedFold' i (s,s') a
-- @
--
-- @
-- 'beside' :: 'IndexPreservingTraversal' s t a b -> 'IndexPreservingTraversal' s' t' a b -> 'IndexPreservingTraversal' (s,s') (t,t') a b
-- 'beside' :: 'IndexPreservingLens' s t a b      -> 'IndexPreservingLens' s' t' a b      -> 'IndexPreservingTraversal' (s,s') (t,t') a b
-- 'beside' :: 'IndexPreservingFold' s a          -> 'IndexPreservingFold' s' a           -> 'IndexPreservingFold' (s,s') a
-- 'beside' :: 'IndexPreservingGetter' s a        -> 'IndexPreservingGetter' s' a         -> 'IndexPreservingFold' (s,s') a
-- @
--
-- >>> ("hello",["world","!!!"])^..beside id traverse
-- ["hello","world","!!!"]
beside :: (Representable q, Applicative (Rep q), Applicative f, Bitraversable r)
       => Optical p q f s t a b
       -> Optical p q f s' t' a b
       -> Optical p q f (r s s') (r t t') a b
beside l r f = tabulate $ getCompose #. bitraverse (Compose #. sieve (l f)) (Compose #. sieve (r f))
{-# INLINE beside #-}

-- | Visit the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
--
-- >>> [("hello","world"),("!!!","!!!")]^.. taking 2 (traverse.both)
-- ["hello","world"]
--
-- >>> timingOut $ [1..] ^.. taking 3 traverse
-- [1,2,3]
--
-- >>> over (taking 5 traverse) succ "hello world"
-- "ifmmp world"
--
-- @
-- 'taking' :: 'Int' -> 'Traversal'' s a                   -> 'Traversal'' s a
-- 'taking' :: 'Int' -> 'Lens'' s a                        -> 'Traversal'' s a
-- 'taking' :: 'Int' -> 'Iso'' s a                         -> 'Traversal'' s a
-- 'taking' :: 'Int' -> 'Prism'' s a                       -> 'Traversal'' s a
-- 'taking' :: 'Int' -> 'Getter' s a                       -> 'Fold' s a
-- 'taking' :: 'Int' -> 'Fold' s a                         -> 'Fold' s a
-- 'taking' :: 'Int' -> 'IndexedTraversal'' i s a          -> 'IndexedTraversal'' i s a
-- 'taking' :: 'Int' -> 'IndexedLens'' i s a               -> 'IndexedTraversal'' i s a
-- 'taking' :: 'Int' -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'taking' :: 'Int' -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- @
taking :: (Conjoined p, Applicative f)
        => Int
       -> Traversing p f s t a a
       -> Over p f s t a a
taking n l = conjoined
  (\ afb s  -> let b = inline l sell s in outs b <$> traverse afb          (take n $ ins b))
  (\ pafb s -> let b = inline l sell s in outs b <$> traverse (cosieve pafb) (take n $ pins b))
{-# INLINE taking #-}

-- | Visit all but the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
--
-- >>> ("hello","world") ^? dropping 1 both
-- Just "world"
--
-- Dropping works on infinite traversals as well:
--
-- >>> [1..] ^? dropping 1 folded
-- Just 2
--
-- @
-- 'dropping' :: 'Int' -> 'Traversal'' s a                   -> 'Traversal'' s a
-- 'dropping' :: 'Int' -> 'Lens'' s a                        -> 'Traversal'' s a
-- 'dropping' :: 'Int' -> 'Iso'' s a                         -> 'Traversal'' s a
-- 'dropping' :: 'Int' -> 'Prism'' s a                       -> 'Traversal'' s a
-- 'dropping' :: 'Int' -> 'Getter' s a                       -> 'Fold' s a
-- 'dropping' :: 'Int' -> 'Fold' s a                         -> 'Fold' s a
-- 'dropping' :: 'Int' -> 'IndexedTraversal'' i s a          -> 'IndexedTraversal'' i s a
-- 'dropping' :: 'Int' -> 'IndexedLens'' i s a               -> 'IndexedTraversal'' i s a
-- 'dropping' :: 'Int' -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'dropping' :: 'Int' -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- @
dropping :: (Conjoined p, Applicative f) => Int -> Over p (Indexing f) s t a a -> Over p f s t a a
dropping n l pafb s = snd $ runIndexing (l paifb s) 0 where
  paifb = cotabulate $ \wa -> Indexing $ \i -> let i' = i + 1 in i' `seq` (i', if i < n then pure (extract wa) else cosieve pafb wa)
{-# INLINE dropping #-}

------------------------------------------------------------------------------
-- Cloning Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' is completely characterized by its behavior on a 'Bazaar'.
--
-- Cloning a 'Traversal' is one way to make sure you aren't given
-- something weaker, such as a 'Fold' and can be
-- used as a way to pass around traversals that have to be monomorphic in @f@.
--
-- Note: This only accepts a proper 'Traversal' (or 'Lens'). To clone a 'Lens'
-- as such, use 'Control.Lens.Lens.cloneLens'.
--
-- Note: It is usually better to use 'Control.Lens.Reified.ReifiedTraversal' and
-- 'Control.Lens.Reified.runTraversal' than to 'cloneTraversal'. The
-- former can execute at full speed, while the latter needs to round trip through
-- the 'Bazaar'.
--
-- >>> let foo l a = (view (getting (cloneTraversal l)) a, set (cloneTraversal l) 10 a)
-- >>> foo both ("hello","world")
-- ("helloworld",(10,10))
--
-- @
-- 'cloneTraversal' :: 'LensLike' ('Bazaar' (->) a b) s t a b -> 'Traversal' s t a b
-- @
cloneTraversal :: ATraversal s t a b -> Traversal s t a b
cloneTraversal l f = bazaar f . l sell
{-# INLINE cloneTraversal #-}

-- | Clone a 'Traversal' yielding an 'IndexPreservingTraversal' that passes through
-- whatever index it is composed with.
cloneIndexPreservingTraversal :: ATraversal s t a b -> IndexPreservingTraversal s t a b
cloneIndexPreservingTraversal l pafb = cotabulate $ \ws -> runBazaar (l sell (extract ws)) $ \a -> cosieve pafb (a <$ ws)
{-# INLINE cloneIndexPreservingTraversal #-}

-- | Clone an 'IndexedTraversal' yielding an 'IndexedTraversal' with the same index.
cloneIndexedTraversal :: AnIndexedTraversal i s t a b -> IndexedTraversal i s t a b
cloneIndexedTraversal l f = bazaar (Indexed (indexed f)) . l sell
{-# INLINE cloneIndexedTraversal #-}

-- | A 'Traversal1' is completely characterized by its behavior on a 'Bazaar1'.
cloneTraversal1 :: ATraversal1 s t a b -> Traversal1 s t a b
cloneTraversal1 l f = bazaar1 f . l sell
{-# INLINE cloneTraversal1 #-}

-- | Clone a 'Traversal1' yielding an 'IndexPreservingTraversal1' that passes through
-- whatever index it is composed with.
cloneIndexPreservingTraversal1 :: ATraversal1 s t a b -> IndexPreservingTraversal1 s t a b
cloneIndexPreservingTraversal1 l pafb = cotabulate $ \ws -> runBazaar1 (l sell (extract ws)) $ \a -> cosieve pafb (a <$ ws)
{-# INLINE cloneIndexPreservingTraversal1 #-}

-- | Clone an 'IndexedTraversal1' yielding an 'IndexedTraversal1' with the same index.
cloneIndexedTraversal1 :: AnIndexedTraversal1 i s t a b -> IndexedTraversal1 i s t a b
cloneIndexedTraversal1 l f = bazaar1 (Indexed (indexed f)) . l sell
{-# INLINE cloneIndexedTraversal1 #-}

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Traversal with an index.
--
-- /NB:/ When you don't need access to the index then you can just apply your 'IndexedTraversal'
-- directly as a function!
--
-- @
-- 'itraverseOf' ≡ 'Control.Lens.Indexed.withIndex'
-- 'Control.Lens.Traversal.traverseOf' l = 'itraverseOf' l '.' 'const' = 'id'
-- @
--
-- @
-- 'itraverseOf' :: 'Functor' f     => 'IndexedLens' i s t a b       -> (i -> a -> f b) -> s -> f t
-- 'itraverseOf' :: 'Applicative' f => 'IndexedTraversal' i s t a b  -> (i -> a -> f b) -> s -> f t
-- 'itraverseOf' :: 'Apply' f       => 'IndexedTraversal1' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
itraverseOf :: (Indexed i a (f b) -> s -> f t) -> (i -> a -> f b) -> s -> f t
itraverseOf l = l .# Indexed
{-# INLINE itraverseOf #-}

-- | Traverse with an index (and the arguments flipped).
--
-- @
-- 'Control.Lens.Traversal.forOf' l a ≡ 'iforOf' l a '.' 'const'
-- 'iforOf' ≡ 'flip' '.' 'itraverseOf'
-- @
--
-- @
-- 'iforOf' :: 'Functor' f     => 'IndexedLens' i s t a b       -> s -> (i -> a -> f b) -> f t
-- 'iforOf' :: 'Applicative' f => 'IndexedTraversal' i s t a b  -> s -> (i -> a -> f b) -> f t
-- 'iforOf' :: 'Apply' f       => 'IndexedTraversal1' i s t a b -> s -> (i -> a -> f b) -> f t
-- @
iforOf :: (Indexed i a (f b) -> s -> f t) -> s -> (i -> a -> f b) -> f t
iforOf = flip . itraverseOf
{-# INLINE iforOf #-}

-- | Map each element of a structure targeted by a 'Lens' to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- When you don't need access to the index 'mapMOf' is more liberal in what it can accept.
--
-- @
-- 'Control.Lens.Traversal.mapMOf' l ≡ 'imapMOf' l '.' 'const'
-- @
--
-- @
-- 'imapMOf' :: 'Monad' m => 'IndexedLens'       i s t a b -> (i -> a -> m b) -> s -> m t
-- 'imapMOf' :: 'Monad' m => 'IndexedTraversal'  i s t a b -> (i -> a -> m b) -> s -> m t
-- 'imapMOf' :: 'Bind'  m => 'IndexedTraversal1' i s t a b -> (i -> a -> m b) -> s -> m t
-- @
imapMOf :: Over (Indexed i) (WrappedMonad m) s t a b  -> (i -> a -> m b) -> s -> m t
imapMOf l cmd = unwrapMonad #. l (WrapMonad #. Indexed cmd)
{-# INLINE imapMOf #-}

-- | Map each element of a structure targeted by a 'Lens' to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'Control.Lens.Traversal.forMOf' l a ≡ 'iforMOf' l a '.' 'const'
-- 'iforMOf' ≡ 'flip' '.' 'imapMOf'
-- @
--
-- @
-- 'iforMOf' :: 'Monad' m => 'IndexedLens' i s t a b      -> s -> (i -> a -> m b) -> m t
-- 'iforMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> s -> (i -> a -> m b) -> m t
-- @
iforMOf :: (Indexed i a (WrappedMonad m b) -> s -> WrappedMonad m t) -> s -> (i -> a -> m b) -> m t
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @
-- 'Control.Lens.Traversal.mapAccumROf' l ≡ 'imapAccumROf' l '.' 'const'
-- @
--
-- @
-- 'imapAccumROf' :: 'IndexedLens' i s t a b      -> (i -> acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'imapAccumROf' :: 'IndexedTraversal' i s t a b -> (i -> acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
imapAccumROf :: Over (Indexed i) (Backwards (State acc)) s t a b -> (i -> acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
imapAccumROf = imapAccumLOf . backwards
{-# INLINE imapAccumROf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @
-- 'Control.Lens.Traversal.mapAccumLOf' l ≡ 'imapAccumLOf' l '.' 'const'
-- @
--
-- @
-- 'imapAccumLOf' :: 'IndexedLens' i s t a b      -> (i -> acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'imapAccumLOf' :: 'IndexedTraversal' i s t a b -> (i -> acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
imapAccumLOf :: Over (Indexed i) (State acc) s t a b -> (i -> acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
imapAccumLOf l f acc0 s = swap (runState (l (Indexed g) s) acc0) where
  g i a = state $ \acc -> swap (f i acc a)
{-# INLINE imapAccumLOf #-}

------------------------------------------------------------------------------
-- Common Indexed Traversals
------------------------------------------------------------------------------

-- | Traverse any 'Traversable' container. This is an 'IndexedTraversal' that is indexed by ordinal position.
traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b
traversed = conjoined traverse (indexing traverse)
{-# INLINE [0] traversed #-}

imapList :: (Int -> a -> b) -> [a] -> [b]
imapList f = go 0
  where
    go i (x:xs) = f i x : go (i+1) xs
    go _ []     = []
{-# INLINE imapList #-}

{-# RULES
"traversed -> mapped"     traversed = sets fmap          :: Functor f => ASetter (f a) (f b) a b;
"traversed -> folded"     traversed = folded             :: Foldable f => Getting (Endo r) (f a) a;
"traversed -> ifolded"    traversed = folded             :: Foldable f => IndexedGetting Int (Endo r) (f a) a;
"traversed -> imapList"   traversed = isets imapList     :: AnIndexedSetter Int [a] [b] a b;
"traversed -> imapSeq"    traversed = isets mapWithIndex :: AnIndexedSetter Int (Seq a) (Seq b) a b;
"traversed -> imapVector" traversed = isets Vector.imap  :: AnIndexedSetter Int (Vector a) (Vector b) a b;
 #-}

-- | Traverse any 'Traversable1' container. This is an 'IndexedTraversal1' that is indexed by ordinal position.
traversed1 :: Traversable1 f => IndexedTraversal1 Int (f a) (f b) a b
traversed1 = conjoined traverse1 (indexing traverse1)
{-# INLINE traversed1 #-}

-- | Traverse any 'Traversable' container. This is an 'IndexedTraversal' that is indexed by ordinal position.
traversed64 :: Traversable f => IndexedTraversal Int64 (f a) (f b) a b
traversed64 = conjoined traverse (indexing64 traverse)
{-# INLINE traversed64 #-}

-- | This is the trivial empty 'Traversal'.
--
-- @
-- 'ignored' :: 'IndexedTraversal' i s s a b
-- @
--
-- @
-- 'ignored' ≡ 'const' 'pure'
-- @
--
-- >>> 6 & ignored %~ absurd
-- 6
ignored :: Applicative f => pafb -> s -> f s
ignored _ = pure
{-# INLINE ignored #-}

-- | Allows 'IndexedTraversal' the value at the smallest index.
class Ord k => TraverseMin k m | m -> k where
  -- | 'IndexedTraversal' of the element with the smallest index.
  traverseMin :: IndexedTraversal' k (m v) v

instance TraverseMin Int IntMap where
  traverseMin f m = case IntMap.minViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMin (const (Just v)) m
#else
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMin (const v) m
#endif
    Nothing     -> pure m
  {-# INLINE traverseMin #-}

instance Ord k => TraverseMin k (Map k) where
  traverseMin f m = case Map.minViewWithKey m of
    Just ((k, a), _) -> indexed f k a <&> \v -> Map.updateMin (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMin #-}

-- | Allows 'IndexedTraversal' of the value at the largest index.
class Ord k => TraverseMax k m | m -> k where
  -- | 'IndexedTraversal' of the element at the largest index.
  traverseMax :: IndexedTraversal' k (m v) v

instance TraverseMax Int IntMap where
  traverseMax f m = case IntMap.maxViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMax (const (Just v)) m
#else
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMax (const v) m
#endif
    Nothing     -> pure m
  {-# INLINE traverseMax #-}

instance Ord k => TraverseMax k (Map k) where
  traverseMax f m = case Map.maxViewWithKey m of
    Just ((k, a), _) -> indexed f k a <&> \v -> Map.updateMax (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMax #-}

-- | Traverse the /nth/ 'elementOf' a 'Traversal', 'Lens' or
-- 'Iso' if it exists.
--
-- >>> [[1],[3,4]] & elementOf (traverse.traverse) 1 .~ 5
-- [[1],[5,4]]
--
-- >>> [[1],[3,4]] ^? elementOf (folded.folded) 1
-- Just 3
--
-- >>> timingOut $ ['a'..] ^?! elementOf folded 5
-- 'f'
--
-- >>> timingOut $ take 10 $ elementOf traverse 3 .~ 16 $ [0..]
-- [0,1,2,16,4,5,6,7,8,9]
--
-- @
-- 'elementOf' :: 'Traversal'' s a -> 'Int' -> 'IndexedTraversal'' 'Int' s a
-- 'elementOf' :: 'Fold' s a       -> 'Int' -> 'IndexedFold' 'Int' s a
-- @
elementOf :: Applicative f
          => LensLike (Indexing f) s t a a
          -> Int
          -> IndexedLensLike Int f s t a a
elementOf l p = elementsOf l (p ==)
{-# INLINE elementOf #-}

-- | Traverse the /nth/ element of a 'Traversable' container.
--
-- @
-- 'element' ≡ 'elementOf' 'traverse'
-- @
element :: Traversable t => Int -> IndexedTraversal' Int (t a) a
element = elementOf traverse
{-# INLINE element #-}

-- | Traverse (or fold) selected elements of a 'Traversal' (or 'Fold') where their ordinal positions match a predicate.
--
-- @
-- 'elementsOf' :: 'Traversal'' s a -> ('Int' -> 'Bool') -> 'IndexedTraversal'' 'Int' s a
-- 'elementsOf' :: 'Fold' s a       -> ('Int' -> 'Bool') -> 'IndexedFold' 'Int' s a
-- @
elementsOf :: Applicative f
           => LensLike (Indexing f) s t a a
           -> (Int -> Bool)
           -> IndexedLensLike Int f s t a a
elementsOf l p iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, if p i then indexed iafb i a else pure a))) s) 0
{-# INLINE elementsOf #-}

-- | Traverse elements of a 'Traversable' container where their ordinal positions match a predicate.
--
-- @
-- 'elements' ≡ 'elementsOf' 'traverse'
-- @
elements :: Traversable t => (Int -> Bool) -> IndexedTraversal' Int (t a) a
elements = elementsOf traverse
{-# INLINE elements #-}

-- | Try to map a function over this 'Traversal', failing if the 'Traversal' has no targets.
--
-- >>> failover (element 3) (*2) [1,2] :: Maybe [Int]
-- Nothing
--
-- >>> failover _Left (*2) (Right 4) :: Maybe (Either Int Int)
-- Nothing
--
-- >>> failover _Right (*2) (Right 4) :: Maybe (Either Int Int)
-- Just (Right 8)
--
-- @
-- 'failover' :: Alternative m => Traversal s t a b -> (a -> b) -> s -> m t
-- @
failover :: Alternative m => LensLike ((,) Any) s t a b -> (a -> b) -> s -> m t
failover l afb s = case l ((,) (Any True) . afb) s of
  (Any True, t)  -> pure t
  (Any False, _) -> Applicative.empty
{-# INLINE failover #-}

-- | Try to map a function which uses the index over this 'IndexedTraversal', failing if the 'IndexedTraversal' has no targets.
--
-- @
-- 'ifailover' :: Alternative m => IndexedTraversal i s t a b -> (i -> a -> b) -> s -> m t
-- @
ifailover :: Alternative m => Over (Indexed i) ((,) Any) s t a b -> (i -> a -> b) -> s -> m t
ifailover l iafb s = case l ((,) (Any True) `rmap` Indexed iafb) s of
  (Any True, t) -> pure t
  (Any False, _) -> Applicative.empty
{-# INLINE ifailover #-}

-- | Try the first 'Traversal' (or 'Fold'), falling back on the second 'Traversal' (or 'Fold') if it returns no entries.
--
-- This is only a valid 'Traversal' if the second 'Traversal' is disjoint from the result of the first or returns
-- exactly the same results. These conditions are trivially met when given a 'Lens', 'Iso', 'Getter', 'Prism' or \"affine\" Traversal -- one that
-- has 0 or 1 target.
--
-- Mutatis mutandis for 'Fold'.
--
-- >>> [0,1,2,3] ^? failing (ix 1) (ix 2)
-- Just 1
--
-- >>> [0,1,2,3] ^? failing (ix 42) (ix 2)
-- Just 2
--
-- @
-- 'failing' :: 'Traversal' s t a b -> 'Traversal' s t a b -> 'Traversal' s t a b
-- 'failing' :: 'Prism' s t a b     -> 'Prism' s t a b     -> 'Traversal' s t a b
-- 'failing' :: 'Fold' s a          -> 'Fold' s a          -> 'Fold' s a
-- @
--
-- These cases are also supported, trivially, but are boring, because the left hand side always succeeds.
--
-- @
-- 'failing' :: 'Lens' s t a b      -> 'Traversal' s t a b -> 'Traversal' s t a b
-- 'failing' :: 'Iso' s t a b       -> 'Traversal' s t a b -> 'Traversal' s t a b
-- 'failing' :: 'Equality' s t a b  -> 'Traversal' s t a b -> 'Traversal' s t a b
-- 'failing' :: 'Getter' s a        -> 'Fold' s a          -> 'Fold' s a
-- @
--
-- If both of the inputs are indexed, the result is also indexed, so you can apply this to a pair of indexed
-- traversals or indexed folds, obtaining an indexed traversal or indexed fold.
--
-- @
-- 'failing' :: 'IndexedTraversal' i s t a b -> 'IndexedTraversal' i s t a b -> 'IndexedTraversal' i s t a b
-- 'failing' :: 'IndexedFold' i s a          -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- @
--
-- These cases are also supported, trivially, but are boring, because the left hand side always succeeds.
--
-- @
-- 'failing' :: 'IndexedLens' i s t a b      -> 'IndexedTraversal' i s t a b -> 'IndexedTraversal' i s t a b
-- 'failing' :: 'IndexedGetter' i s a        -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- @
failing :: (Conjoined p, Applicative f) => Traversing p f s t a b -> Over p f s t a b -> Over p f s t a b
failing l r pafb s = case pins b of
  [] -> r pafb s
  _  -> bazaar pafb b
  where b = l sell s

infixl 5 `failing`

-- | Try the second traversal. If it returns no entries, try again with all entries from the first traversal, recursively.
--
-- @
-- 'deepOf' :: 'Fold' s s          -> 'Fold' s a                   -> 'Fold' s a
-- 'deepOf' :: 'Traversal'' s s    -> 'Traversal'' s a             -> 'Traversal'' s a
-- 'deepOf' :: 'Traversal' s t s t -> 'Traversal' s t a b          -> 'Traversal' s t a b
-- 'deepOf' :: 'Fold' s s          -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'deepOf' :: 'Traversal' s t s t -> 'IndexedTraversal' i s t a b -> 'IndexedTraversal' i s t a b
-- @
deepOf :: (Conjoined p, Applicative f) => LensLike f s t s t -> Traversing p f s t a b -> Over p f s t a b
deepOf r l = failing l (r . deepOf r l)

-- | "Fuse" a 'Traversal' by reassociating all of the @('<*>')@ operations to the
-- left and fusing all of the 'fmap' calls into one. This is particularly
-- useful when constructing a 'Traversal' using operations from "GHC.Generics".
--
-- Given a pair of 'Traversal's 'foo' and 'bar',
--
-- @
-- 'confusing' (foo.bar) = foo.bar
-- @
--
-- However, @foo@ and @bar@ are each going to use the 'Applicative' they are given.
--
-- 'confusing' exploits the 'Yoneda' lemma to merge their separate uses of 'fmap' into a single 'fmap'.
-- and it further exploits an interesting property of the right Kan lift (or 'Curried') to left associate
-- all of the uses of @('<*>')@ to make it possible to fuse together more fmaps.
--
-- This is particularly effective when the choice of functor 'f' is unknown at compile
-- time or when the 'Traversal' @foo.bar@ in the above description is recursive or complex
-- enough to prevent inlining.
--
-- 'Control.Lens.Lens.fusing' is a version of this combinator suitable for fusing lenses.
--
-- @
-- 'confusing' :: 'Traversal' s t a b -> 'Traversal' s t a b
-- @
confusing :: Applicative f => LensLike (Curried (Yoneda f) (Yoneda f)) s t a b -> LensLike f s t a b
confusing t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
  where
  liftCurriedYoneda :: Applicative f => f a -> Curried (Yoneda f) (Yoneda f) a
  liftCurriedYoneda fa = Curried (`yap` fa)
  {-# INLINE liftCurriedYoneda #-}

  yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
  yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
  {-# INLINE yap #-}

{-# INLINE confusing #-}

-- | Traverse a container using a specified 'Applicative'.
--
-- This is like 'traverseBy' where the 'Traversable' instance can be specified by any 'Traversal'
--
-- @
-- 'traverseByOf' 'traverse' ≡ 'traverseBy'
-- @
traverseByOf :: Traversal s t a b -> (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> (a -> f b) -> s -> f t
traverseByOf l pur app f = reifyApplicative pur app (l (ReflectedApplicative #. f))

-- | Sequence a container using a specified 'Applicative'.
--
-- This is like 'traverseBy' where the 'Traversable' instance can be specified by any 'Traversal'
--
-- @
-- 'sequenceByOf' 'traverse' ≡ 'sequenceBy'
-- @
sequenceByOf :: Traversal s t (f b) b -> (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> s -> f t
sequenceByOf l pur app = reifyApplicative pur app (l ReflectedApplicative)
