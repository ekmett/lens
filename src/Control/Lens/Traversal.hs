{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Traversal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Traversal' s t a b@ is a generalization of 'traverse' from
-- 'Traversable'. It allows you to traverse over a structure and change out
-- its contents with monadic or applicative side-effects. Starting from
--
-- @'traverse' :: ('Traversable' t, 'Applicative' f) => (a -> f b) -> t a -> f (t b)@,
--
-- we monomorphize the contents and result to obtain
--
--  > type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
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

  -- * Traversing and Lensing
  , traverseOf, forOf, sequenceAOf
  , mapMOf, forMOf, sequenceOf
  , transposeOf
  , mapAccumLOf, mapAccumROf
  , scanr1Of, scanl1Of

  -- * Common Traversals
  , Traversable(traverse)
  , ignored
  , traverseLeft
  , traverseRight
  , both
  , taking
  , dropping

  -- * Cloning Traversals
  , cloneTraversal
  , ReifiedTraversal(..)

  -- * Simple
  , SimpleTraversal
  , SimpleReifiedTraversal
  ) where

import Control.Applicative              as Applicative
import Control.Applicative.Backwards
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.State.Class        as State
import Control.Monad.Trans.State.Lazy   as Lazy
import Data.Traversable

-- $setup
-- >>> import Control.Lens

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
-- 'Lens' or 'Control.Lens.Iso.Iso' as a 'Traversal', and composition of a 'Traversal' (or 'Lens' or 'Control.Lens.Iso.Iso') with a 'Traversal' (or 'Lens' or 'Control.Lens.Iso.Iso')
-- using (.) forms a valid 'Traversal'.
--
-- The laws for a Traversal @t@ follow from the laws for Traversable as stated in \"The Essence of the Iterator Pattern\".
--
-- @
-- t 'pure' ≡ 'pure'
-- 'fmap' (t f) '.' t g ≡ 'Data.Functor.Compose.getCompose' '.' t ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- One consequence of this requirement is that a 'Traversal' needs to leave the same number of elements as a
-- candidate for subsequent 'Traversal' that it started with. Another testament to the strength of these laws
-- is that the caveat expressed in section 5.5 of the \"Essence of the Iterator Pattern\" about exotic
-- 'Traversable' instances that 'traverse' the same entry multiple times was actually already ruled out by the
-- second law in that same paper!
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | @type SimpleTraversal = 'Simple' 'Traversal'@
type SimpleTraversal s a = Traversal s s a a

--------------------------
-- Traversal Combinators
--------------------------

-- |
-- Map each element of a structure targeted by a Lens or Traversal,
-- evaluate these actions from left to right, and collect the results.
--
-- This function is only provided for consistency, 'id' is strictly more general.
--
-- @'traverseOf' ≡ 'id'@
--
-- This yields the obvious law:
--
-- @'traverse' ≡ 'traverseOf' 'traverse'@
--
-- @
-- 'traverseOf' :: 'Control.Lens.Iso.Iso' s t a b       -> (a -> f b) -> s -> f t
-- 'traverseOf' :: 'Lens' s t a b      -> (a -> f b) -> s -> f t
-- 'traverseOf' :: 'Traversal' s t a b -> (a -> f b) -> s -> f t
-- @
traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
traverseOf = id
{-# INLINE traverseOf #-}

-- | A version of 'traverseOf' with the arguments flipped, such that:
--
-- @'forOf' l ≡ 'flip' ('traverseOf' l)@
--
-- @
-- 'for' ≡ 'forOf' 'traverse'
-- @
--
-- This function is only provided for consistency, 'flip' is strictly more general.
--
-- @
-- 'forOf' ≡ 'flip'
-- @
--
-- @
-- 'forOf' :: 'Control.Lens.Iso.Iso' s t a b -> s -> (a -> f b) -> f t
-- 'forOf' :: 'Lens' s t a b -> s -> (a -> f b) -> f t
-- 'forOf' :: 'Traversal' s t a b -> s -> (a -> f b) -> f t
-- @
forOf :: LensLike f s t a b -> s -> (a -> f b) -> f t
forOf = flip
{-# INLINE forOf #-}

-- |
-- Evaluate each action in the structure from left to right, and collect
-- the results.
--
-- @
-- 'sequenceA' ≡ 'sequenceAOf' 'traverse' ≡ 'traverse' 'id'
-- 'sequenceAOf' l ≡ 'traverseOf' l id ≡ l id
-- @
--
-- @
-- 'sequenceAOf' ::                  'Control.Lens.Iso.Iso' s t (f b) b       -> s -> f t
-- 'sequenceAOf' ::                  'Lens' s t (f b) b      -> s -> f t
-- 'sequenceAOf' :: 'Applicative' f => 'Traversal' s t (f b) b -> s -> f t
-- @
sequenceAOf :: LensLike f s t (f b) b -> s -> f t
sequenceAOf l = l id
{-# INLINE sequenceAOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results.
--
-- @'mapM' ≡ 'mapMOf' 'traverse'@
--
-- @
-- 'mapMOf' ::            'Control.Lens.Iso.Iso' s t a b       -> (a -> m b) -> s -> m t
-- 'mapMOf' ::            'Lens' s t a b      -> (a -> m b) -> s -> m t
-- 'mapMOf' :: 'Monad' m => 'Traversal' s t a b -> (a -> m b) -> s -> m t
-- @
mapMOf :: LensLike (WrappedMonad m) s t a b -> (a -> m b) -> s -> m t
mapMOf l cmd = unwrapMonad# (l (wrapMonad# cmd))
{-# INLINE mapMOf #-}

-- | 'forMOf' is a flipped version of 'mapMOf', consistent with the definition of 'forM'.
-- @
-- 'forM' ≡ 'forMOf' 'traverse'
-- 'forMOf' l ≡ 'flip' ('mapMOf' l)
-- @
--
-- @
-- 'forMOf' ::            'Control.Lens.Iso.Iso' s t a b       -> s -> (a -> m b) -> m t
-- 'forMOf' ::            'Lens' s t a b      -> s -> (a -> m b) -> m t
-- 'forMOf' :: 'Monad' m => 'Traversal' s t a b -> s -> (a -> m b) -> m t
-- @
forMOf :: LensLike (WrappedMonad m) s t a b -> s -> (a -> m b) -> m t
forMOf l a cmd = unwrapMonad (l (wrapMonad# cmd) a)
{-# INLINE forMOf #-}

-- | Sequence the (monadic) effects targeted by a lens in a container from left to right.
--
-- @
-- 'sequence' ≡ 'sequenceOf' 'traverse'
-- 'sequenceOf' l ≡ 'mapMOf' l id
-- 'sequenceOf' l ≡ 'unwrapMonad' . l 'WrapMonad'
-- @
--
-- @
-- 'sequenceOf' ::            'Control.Lens.Iso.Iso' s t (m b) b       -> s -> m t
-- 'sequenceOf' ::            'Lens' s t (m b) b      -> s -> m t
-- 'sequenceOf' :: 'Monad' m => 'Traversal' s t (m b) b -> s -> m t
-- @
sequenceOf :: LensLike (WrappedMonad m) s t (m b) b -> s -> m t
sequenceOf l = unwrapMonad# (l WrapMonad)
{-# INLINE sequenceOf #-}

-- | This generalizes 'Data.List.transpose' to an arbitrary 'Traversal'.
--
-- Note: 'Data.List.transpose' handles ragged inputs more intelligently, but for non-ragged inputs:
--
-- @'Data.List.transpose' ≡ 'transposeOf' 'traverse'@
--
-- >>> transposeOf traverse [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
--
-- Since every 'Lens' is a 'Traversal', we can use this as a form of
-- monadic strength as well:
--
-- @'transposeOf' '_2' :: (b, [a]) -> [(b, a)]@
transposeOf :: LensLike ZipList s t [a] a -> s -> [t]
transposeOf l = getZipList# (l ZipList)
{-# INLINE transposeOf #-}

-- | This generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal'.
--
-- @'mapAccumR' ≡ 'mapAccumROf' 'traverse'@
--
-- 'mapAccumROf' accumulates state from right to left.
--
-- @
-- 'mapAccumROf' :: 'Control.Lens.Iso.Iso' s t a b       -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumROf' :: 'Lens' s t a b      -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumROf' :: 'Traversal' s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
mapAccumROf :: LensLike (Lazy.State acc) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumROf l f s0 a = swap (Lazy.runState (l (\c -> State.state (\s -> swap (f s c))) a) s0)
{-# INLINE mapAccumROf #-}

-- | This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'.
--
-- @'mapAccumL' ≡ 'mapAccumLOf' 'traverse'@
--
-- 'mapAccumLOf' accumulates state from left to right.
--
-- @
-- 'mapAccumLOf' :: 'Control.Lens.Iso.Iso' s t a b       -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' :: 'Lens' s t a b      -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' :: 'Traversal' s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
mapAccumLOf :: LensLike (Backwards (Lazy.State acc)) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumLOf = mapAccumROf . backwards
{-# INLINE mapAccumLOf #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

-- | This permits the use of 'scanr1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanr1' ≡ 'scanr1Of' 'traverse'@
--
-- @
-- 'scanr1Of' :: 'Control.Lens.Iso.Iso' s t a a       -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Lens' s t a a      -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Traversal' s t a a -> (a -> a -> a) -> s -> t
-- @
scanr1Of :: LensLike (Lazy.State (Maybe a)) s t a a -> (a -> a -> a) -> s -> t
scanr1Of l f = snd . mapAccumROf l step Nothing where
  step Nothing a  = (Just a, a)
  step (Just s) a = (Just r, r) where r = f a s
{-# INLINE scanr1Of #-}

-- | This permits the use of 'scanl1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanl1' ≡ 'scanl1Of' 'traverse'@
--
-- @
-- 'scanr1Of' :: 'Control.Lens.Iso.Iso' s t a a       -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Lens' s t a a      -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Traversal' s t a a -> (a -> a -> a) -> s -> t
-- @
scanl1Of :: LensLike (Backwards (Lazy.State (Maybe a))) s t a a -> (a -> a -> a) -> s -> t
scanl1Of l f = snd . mapAccumLOf l step Nothing where
  step Nothing a  = (Just a, a)
  step (Just s) a = (Just r, r) where r = f s a
{-# INLINE scanl1Of #-}

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | This is the trivial empty traversal.
--
-- @'ignored' :: 'Applicative' f => (a -> f b) -> s -> f s@
--
-- @'ignored' ≡ 'const' 'pure'@
ignored :: Traversal s s a b
ignored _ = pure
{-# INLINE ignored #-}

-- | Traverse both parts of a tuple with matching types.
--
-- >>> both *~ 10 $ (1,2)
-- (10,20)
-- >>> over both length ("hello","world")
-- (5,5)
-- >>> ("hello","world")^.both
-- "helloworld"
both :: Traversal (a,a) (b,b) a b
both f (a,a') = (,) <$> f a <*> f a'
{-# INLINE both #-}

-- | A traversal for tweaking the left-hand value of an 'Either':
--
-- >>> over traverseLeft (+1) (Left 2)
-- Left 3
-- >>> over traverseLeft (+1) (Right 2)
-- Right 2
-- >>> Right 42 ^.traverseLeft :: String
-- ""
-- >>> Left "hello" ^.traverseLeft
-- "hello"
--
-- @traverseLeft :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ('Either' b c)@
traverseLeft :: Traversal (Either a c) (Either b c) a b
traverseLeft f (Left a)  = Left <$> f a
traverseLeft _ (Right c) = pure $ Right c
{-# INLINE traverseLeft #-}

-- | traverse the right-hand value of an 'Either':
--
-- @'traverseRight' ≡ 'Data.Traversable.traverse'@
--
-- Unfortunately the instance for
-- @'Data.Traversable.Traversable' ('Either' c)@ is still missing from base,
-- so this can't just be 'Data.Traversable.traverse'
--
-- >>> over traverseRight (+1) (Left 2)
-- Left 2
-- >>> over traverseRight (+1) (Right 2)
-- Right 3
-- >>> Right "hello" ^.traverseRight
-- "hello"
-- >>> Left "hello" ^.traverseRight :: [Double]
-- []
--
-- @traverseRight :: 'Applicative' f => (a -> f b) -> 'Either' c a -> f ('Either' c a)@
traverseRight :: Traversal (Either c a) (Either c b) a b
traverseRight _ (Left c) = pure $ Left c
traverseRight f (Right a) = Right <$> f a
{-# INLINE traverseRight #-}

-- | Visit the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
taking :: Applicative f => Int -> SimpleLensLike (Indexing f) s a -> SimpleLensLike f s a
taking n l f s = case runIndexing (l (\a -> Indexing $ \i -> IndexingResult (if i < n then f a else pure a) (i + 1)) s) 0 of
  IndexingResult r _ -> r
{-# INLINE taking #-}

-- | Visit all but the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
dropping :: Applicative f => Int -> SimpleLensLike (Indexing f) s a -> SimpleLensLike f s a
dropping n l f s = case runIndexing (l (\a -> Indexing $ \i -> IndexingResult (if i >= n then f a else pure a) (i + 1)) s) 0 of
  IndexingResult r _ -> r
{-# INLINE dropping #-}

------------------------------------------------------------------------------
-- Cloning Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' is completely characterized by its behavior on a 'Bazaar'.
--
-- Cloning a 'Traversal' is one way to make sure you aren't given
-- something weaker, such as a 'Control.Lens.Traversal.Fold' and can be
-- used as a way to pass around traversals that have to be monomorphic in @f@.
--
-- Note: This only accepts a proper 'Traversal' (or 'Lens'). To clone a 'Lens'
-- as such, use 'cloneLens'
--
-- Note: It is usually better to 'ReifyTraversal' and use 'reflectTraversal'
-- than to 'cloneTraversal'. The former can execute at full speed, while the
-- latter needs to round trip through the 'Bazaar'.
--
-- >>> let foo l a = (view (cloneTraversal l) a, set (cloneTraversal l) 10 a)
-- >>> foo both ("hello","world")
-- ("helloworld",(10,10))
--
-- @'cloneTraversal' :: 'LensLike' ('Bazaar' a b) s t a b -> 'Traversal' s t a b@
cloneTraversal :: Applicative f => ((a -> Bazaar a b b) -> s -> Bazaar a b t) -> (a -> f b) -> s -> f t
cloneTraversal l f = bazaar f . l sell
{-# INLINE cloneTraversal #-}

-- | A form of 'Traversal' that can be stored monomorphically in a container.
data ReifiedTraversal s t a b = ReifyTraversal { reflectTraversal :: Traversal s t a b }

-- | @type SimpleReifiedTraversal = 'Simple' 'ReifiedTraversal'@
type SimpleReifiedTraversal s a = ReifiedTraversal s s a a
