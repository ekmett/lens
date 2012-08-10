{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Traversal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Traversal' a b c d@ is a generalization of 'traverse' from
-- 'Traversable'. It allows you to traverse over a structure and change out
-- its contents with monadic or applicative side-effects. Starting from
--
-- @'traverse' :: ('Traversable' t, 'Applicative' f) => (c -> f d) -> t c -> f (t d)@,
--
-- we monomorphize the contents and result to obtain
--
--  > type Traversal a b c d = forall f. Applicative f => (c -> f d) -> a -> f b
--
-- While a 'Traversal' isn't quite a 'Fold', it _can_ be used for 'Getting'
-- like a 'Fold', because given a 'Monoid' @m@, we have an 'Applicative'
-- for @('Const' m)@. Everything you know how to do with a 'Traversable'
-- container, you can with with a 'Traversal', and here we provide
-- combinators that generalize the usual 'Traversable' operations.
----------------------------------------------------------------------------
module Control.Lens.Traversal
  (
  -- * Lenses
    Traversal

  -- ** Lensing Traversals
  , element
  , elementOf

  -- * Traversing and Lensing
  , traverseOf, forOf, sequenceAOf
  , mapMOf, forMOf, sequenceOf
  , transposeOf
  , mapAccumLOf, mapAccumROf
  , scanr1Of, scanl1Of

  -- * Common Traversals
  , Traversable(traverse)
  , traverseNothing

  -- * Simple
  , SimpleTraversal
  ) where

import Control.Applicative              as Applicative
import Control.Applicative.Backwards
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.State.Class        as State
import Control.Monad.Trans.State.Lazy   as Lazy
import Data.Traversable

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' can be used directly as a 'Control.Lens.Setter.Setter' or a 'Fold' (but not as a 'Lens') and provides
-- the ability to both read and update multiple fields, subject to some relatively weak 'Traversal' laws.
--
-- These have also been known as multilenses, but they have the signature and spirit of
--
-- @'traverse' :: 'Traversable' f => 'Traversal' (f a) (f b) a b@
--
-- and the more evocative name suggests their application.
--
-- Most of the time the 'Traversal' you will want to use is just 'traverse', but you can also pass any
-- 'Lens' or 'Control.Lens.Iso.Iso' as a Traversal, and composition of a 'Traversal' (or 'Lens' or 'Control.Lens.Iso.Iso') with a 'Traversal' (or 'Lens' or 'Control.Lens.Iso.Iso')
-- using (.) forms a valid 'Traversal'.
--
-- The laws for a Traversal @t@ follow from the laws for Traversable as stated in \"The Essence of the Iterator Pattern\".
--
-- 1) Idiomatic naturality:
--
-- @t 'pure' = 'pure'@
--
-- 2) Sequential composition:
--
-- @'fmap' (t f) . t g = 'Data.Functor.Compose.getCompose' . t ('Data.Functor.Compose.Compose' . 'fmap' f . g)@
--
-- One consequence of this requirement is that a traversal needs to leave the same number of elements as a candidate for
-- subsequent traversal as it started with.
--
-- 3) No duplication of elements (as defined in \"The Essence of the Iterator Pattern\" section 5.5), which states
-- that you should incur no effect caused by visiting the same element of the container twice.
type Traversal a b c d = forall f. Applicative f => (c -> f d) -> a -> f b

-- | @type SimpleTraversal = 'Simple' 'Traversal'@
type SimpleTraversal a b = Traversal a a b b

--------------------------
-- Traversal Combinators
--------------------------

-- |
-- Map each element of a structure targeted by a Lens or Traversal,
-- evaluate these actions from left to right, and collect the results.
--
-- @'traverseOf' = 'id'@
--
-- @'traverse' = 'traverseOf' 'traverse'@
--
-- @
-- 'traverseOf' :: 'Control.Lens.Iso.Iso' a b c d       -> (c -> f d) -> a -> f b
-- 'traverseOf' :: 'Lens' a b c d      -> (c -> f d) -> a -> f b
-- 'traverseOf' :: 'Traversal' a b c d -> (c -> f d) -> a -> f b
-- @
traverseOf :: LensLike f a b c d -> (c -> f d) -> a -> f b
traverseOf = id
{-# INLINE traverseOf #-}

-- |
--
-- @'forOf' l = 'flip' ('traverseOf' l)@
--
-- @
-- 'for' = 'forOf' 'traverse'
-- 'forOf' = 'flip'
-- @
--
-- @
-- forOf :: 'Control.Lens.Iso.Iso' a b c d -> a -> (c -> f d) -> f b
-- forOf :: 'Lens' a b c d -> a -> (c -> f d) -> f b
-- forOf :: 'Traversal' a b c d -> a -> (c -> f d) -> f b
-- @
forOf :: LensLike f a b c d -> a -> (c -> f d) -> f b
forOf = flip
{-# INLINE forOf #-}

-- |
-- Evaluate each action in the structure from left to right, and collect
-- the results.
--
-- @
-- 'sequenceA' = 'sequenceAOf' 'traverse' = 'traverse' 'id'
-- 'sequenceAOf' l = 'traverseOf' l id
-- 'sequenceAOf' l = l id
-- @
--
-- @
-- 'sequenceAOf' ::                  'Control.Lens.Iso.Iso' a b (f c) c       -> a -> f b
-- 'sequenceAOf' ::                  'Lens' a b (f c) c      -> a -> f b
-- 'sequenceAOf' :: 'Applicative' f => 'Traversal' a b (f c) c -> a -> f b
-- @
sequenceAOf :: LensLike f a b (f c) c -> a -> f b
sequenceAOf l = l id
{-# INLINE sequenceAOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results.
--
-- @'mapM' = 'mapMOf' 'traverse'@
--
-- @
-- 'mapMOf ::            'Control.Lens.Iso.Iso' a b c d       -> (c -> m d) -> a -> m b
-- 'mapMOf ::            'Lens' a b c d      -> (c -> m d) -> a -> m b
-- 'mapMOf :: 'Monad' m => 'Traversal' a b c d -> (c -> m d) -> a -> m b
-- @
mapMOf :: LensLike (WrappedMonad m) a b c d -> (c -> m d) -> a -> m b
mapMOf l cmd = unwrapMonad . l (WrapMonad . cmd)
{-# INLINE mapMOf #-}

-- |
-- @
-- 'forM' = 'forMOf' 'traverse'
-- 'forMOf' l = 'flip' ('mapMOf' l)
-- @
--
-- @
-- forMOf ::            'Control.Lens.Iso.Iso' a b c d       -> a -> (c -> m d) -> m b
-- forMOf ::            'Lens' a b c d      -> a -> (c -> m d) -> m b
-- forMOf :: 'Monad' m => 'Traversal' a b c d -> a -> (c -> m d) -> m b
-- @
forMOf :: LensLike (WrappedMonad m) a b c d -> a -> (c -> m d) -> m b
forMOf l a cmd = unwrapMonad (l (WrapMonad . cmd) a)
{-# INLINE forMOf #-}

-- |
-- @
-- 'sequence' = 'sequenceOf' 'traverse'
-- sequenceOf l = 'mapMOf' l id
-- sequenceOf l = 'unwrapMonad' . l 'WrapMonad'
-- @
--
-- @
-- sequenceOf ::            'Control.Lens.Iso.Iso' a b (m c) c       -> a -> m b
-- sequenceOf ::            'Lens' a b (m c) c      -> a -> m b
-- sequenceOf :: 'Monad' m => 'Traversal' a b (m c) c -> a -> m b
-- @
sequenceOf :: LensLike (WrappedMonad m) a b (m c) c -> a -> m b
sequenceOf l = unwrapMonad . l WrapMonad
{-# INLINE sequenceOf #-}

-- | This generalizes 'Data.List.transpose' to an arbitrary 'Traversal'.
--
-- Note: 'Data.List.transpose' handles ragged inputs more intelligently, but for non-ragged inputs:
--
-- @'Data.List.transpose' = 'transposeOf' 'traverse'@
--
-- >>> transposeOf traverse [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
--
-- Since every 'Lens' is a 'Traversal', we can use this as a form of
-- monadic strength as well:
--
-- @'transposeOf' '_2' :: (b, [a]) -> [(b, a)]@
transposeOf :: LensLike ZipList a b [c] c -> a -> [b]
transposeOf l = getZipList . l ZipList
{-# INLINE transposeOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal'.
--
-- @'mapAccumR' = 'mapAccumROf' 'traverse'@
--
-- 'mapAccumROf' accumulates state from right to left.
--
-- @
-- mapAccumROf :: 'Control.Lens.Iso.Iso' a b c d       -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- mapAccumROf :: 'Lens' a b c d      -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- mapAccumROf :: 'Traversal' a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
mapAccumROf :: LensLike (Lazy.State s) a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
mapAccumROf l f s0 a = swap (Lazy.runState (l (\c -> State.state (\s -> swap (f s c))) a) s0)
{-# INLINE mapAccumROf #-}

-- | Generalized 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'.
--
-- @'mapAccumL' = 'mapAccumLOf' 'traverse'@
--
-- 'mapAccumLOf' accumulates state from left to right.
--
-- @
-- mapAccumLOf :: 'Control.Lens.Iso.Iso' a b c d       -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- mapAccumLOf :: 'Lens' a b c d      -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- mapAccumLOf :: 'Traversal' a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
mapAccumLOf :: LensLike (Backwards (Lazy.State s)) a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
mapAccumLOf = mapAccumROf . backwards
{-# INLINE mapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

-- | Permit the use of 'scanr1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanr1' = 'scanr1Of' 'traverse'@
--
-- @
-- scanr1Of :: 'Control.Lens.Iso.Iso' a b c c       -> (c -> c -> c) -> a -> b
-- scanr1Of :: 'Lens' a b c c      -> (c -> c -> c) -> a -> b
-- scanr1Of :: 'Traversal' a b c c -> (c -> c -> c) -> a -> b
-- @
scanr1Of :: LensLike (Lazy.State (Maybe c)) a b c c -> (c -> c -> c) -> a -> b
scanr1Of l f = snd . mapAccumROf l step Nothing where
  step Nothing c  = (Just c, c)
  step (Just s) c = (Just r, r) where r = f c s
{-# INLINE scanr1Of #-}

-- | Permit the use of 'scanl1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanl1' = 'scanl1Of' 'traverse'@
--
-- @
-- scanr1Of :: Iso a b c c       -> (c -> c -> c) -> a -> b
-- scanr1Of :: Lens a b c c      -> (c -> c -> c) -> a -> b
-- scanr1Of :: Traversal a b c c -> (c -> c -> c) -> a -> b
-- @
scanl1Of :: LensLike (Backwards (Lazy.State (Maybe c))) a b c c -> (c -> c -> c) -> a -> b
scanl1Of l f = snd . mapAccumLOf l step Nothing where
  step Nothing c  = (Just c, c)
  step (Just s) c = (Just r, r) where r = f s c
{-# INLINE scanl1Of #-}

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

-- | A 'Lens' to view/edit the nth element 'elementOf' a 'Traversal', 'Lens' or 'Control.Lens.Iso.Iso'.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- >>> import Control.Lens
-- >>> [[1],[3,4]]^.elementOf (traverse.traverse) 1
-- 3
elementOf :: Functor f => LensLike (ElementOf f) a b c c -> Int -> LensLike f a b c c
elementOf l i f a = case getElementOf (l go a) 0 of
    Found _ fb    -> fb
    Searching _ _ -> error "elementOf: index out of range"
    NotFound e    -> error $ "elementOf: " ++ e
  where
    go c = ElementOf $ \j -> if i == j then Found (j + 1) (f c) else Searching (j + 1) c

-- | Access the nth element of a 'Traversable' container.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- @'element' = 'elementOf' 'traverse'@
element :: Traversable t => Int -> Simple Lens (t a) a
element = elementOf traverse

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | This is the traversal that just doesn't return anything
--
-- @'traverseNothing' :: 'Applicative' f => (c -> f d) -> a -> f a@
traverseNothing :: Traversal a a c d
traverseNothing = const pure
{-# INLINE traverseNothing #-}

