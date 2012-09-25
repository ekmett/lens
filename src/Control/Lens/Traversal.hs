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
  , (:=>)

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
  , ignored
  , traverseLeft
  , traverseRight
  , both

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

infixr 0 :=>

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
type Traversal a b c d = forall f. Applicative f => (c -> f d) -> a -> f b

-- | @type SimpleTraversal = 'Simple' 'Traversal'@
type SimpleTraversal a b = Traversal a a b b

-- | This is a commonly-used infix alias for a @'Simple' 'Traversal'@.
type a :=> b = forall f. Applicative f => (b -> f b) -> a -> f a

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
-- 'traverseOf' :: 'Control.Lens.Iso.Iso' a b c d       -> (c -> f d) -> a -> f b
-- 'traverseOf' :: 'Lens' a b c d      -> (c -> f d) -> a -> f b
-- 'traverseOf' :: 'Traversal' a b c d -> (c -> f d) -> a -> f b
-- @
traverseOf :: LensLike f a b c d -> (c -> f d) -> a -> f b
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
-- 'forOf' :: 'Control.Lens.Iso.Iso' a b c d -> a -> (c -> f d) -> f b
-- 'forOf' :: 'Lens' a b c d -> a -> (c -> f d) -> f b
-- 'forOf' :: 'Traversal' a b c d -> a -> (c -> f d) -> f b
-- @
forOf :: LensLike f a b c d -> a -> (c -> f d) -> f b
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
-- @'mapM' ≡ 'mapMOf' 'traverse'@
--
-- @
-- 'mapMOf' ::            'Control.Lens.Iso.Iso' a b c d       -> (c -> m d) -> a -> m b
-- 'mapMOf' ::            'Lens' a b c d      -> (c -> m d) -> a -> m b
-- 'mapMOf' :: 'Monad' m => 'Traversal' a b c d -> (c -> m d) -> a -> m b
-- @
mapMOf :: LensLike (WrappedMonad m) a b c d -> (c -> m d) -> a -> m b
mapMOf l cmd = unwrapMonad . l (WrapMonad . cmd)
{-# INLINE mapMOf #-}

-- | 'forMOf' is a flipped version of 'mapMOf', consistent with the definition of 'forM'.
-- @
-- 'forM' ≡ 'forMOf' 'traverse'
-- 'forMOf' l ≡ 'flip' ('mapMOf' l)
-- @
--
-- @
-- 'forMOf' ::            'Control.Lens.Iso.Iso' a b c d       -> a -> (c -> m d) -> m b
-- 'forMOf' ::            'Lens' a b c d      -> a -> (c -> m d) -> m b
-- 'forMOf' :: 'Monad' m => 'Traversal' a b c d -> a -> (c -> m d) -> m b
-- @
forMOf :: LensLike (WrappedMonad m) a b c d -> a -> (c -> m d) -> m b
forMOf l a cmd = unwrapMonad (l (WrapMonad . cmd) a)
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
-- 'sequenceOf' ::            'Control.Lens.Iso.Iso' a b (m c) c       -> a -> m b
-- 'sequenceOf' ::            'Lens' a b (m c) c      -> a -> m b
-- 'sequenceOf' :: 'Monad' m => 'Traversal' a b (m c) c -> a -> m b
-- @
sequenceOf :: LensLike (WrappedMonad m) a b (m c) c -> a -> m b
sequenceOf l = unwrapMonad . l WrapMonad
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
transposeOf :: LensLike ZipList a b [c] c -> a -> [b]
transposeOf l = getZipList . l ZipList
{-# INLINE transposeOf #-}

-- | This generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal'.
--
-- @'mapAccumR' ≡ 'mapAccumROf' 'traverse'@
--
-- 'mapAccumROf' accumulates state from right to left.
--
-- @
-- 'mapAccumROf' :: 'Control.Lens.Iso.Iso' a b c d       -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- 'mapAccumROf' :: 'Lens' a b c d      -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- 'mapAccumROf' :: 'Traversal' a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
mapAccumROf :: LensLike (Lazy.State s) a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
mapAccumROf l f s0 a = swap (Lazy.runState (l (\c -> State.state (\s -> swap (f s c))) a) s0)
{-# INLINE mapAccumROf #-}

-- | This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'.
--
-- @'mapAccumL' ≡ 'mapAccumLOf' 'traverse'@
--
-- 'mapAccumLOf' accumulates state from left to right.
--
-- @
-- 'mapAccumLOf' :: 'Control.Lens.Iso.Iso' a b c d       -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- 'mapAccumLOf' :: 'Lens' a b c d      -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- 'mapAccumLOf' :: 'Traversal' a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
-- @
mapAccumLOf :: LensLike (Backwards (Lazy.State s)) a b c d -> (s -> c -> (s, d)) -> s -> a -> (s, b)
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
-- 'scanr1Of' :: 'Control.Lens.Iso.Iso' a b c c       -> (c -> c -> c) -> a -> b
-- 'scanr1Of' :: 'Lens' a b c c      -> (c -> c -> c) -> a -> b
-- 'scanr1Of' :: 'Traversal' a b c c -> (c -> c -> c) -> a -> b
-- @
scanr1Of :: LensLike (Lazy.State (Maybe c)) a b c c -> (c -> c -> c) -> a -> b
scanr1Of l f = snd . mapAccumROf l step Nothing where
  step Nothing c  = (Just c, c)
  step (Just s) c = (Just r, r) where r = f c s
{-# INLINE scanr1Of #-}

-- | This permits the use of 'scanl1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanl1' ≡ 'scanl1Of' 'traverse'@
--
-- @
-- 'scanr1Of' :: 'Control.Lens.Iso.Iso' a b c c       -> (c -> c -> c) -> a -> b
-- 'scanr1Of' :: 'Lens' a b c c      -> (c -> c -> c) -> a -> b
-- 'scanr1Of' :: 'Traversal' a b c c -> (c -> c -> c) -> a -> b
-- @
scanl1Of :: LensLike (Backwards (Lazy.State (Maybe c))) a b c c -> (c -> c -> c) -> a -> b
scanl1Of l f = snd . mapAccumLOf l step Nothing where
  step Nothing c  = (Just c, c)
  step (Just s) c = (Just r, r) where r = f s c
{-# INLINE scanl1Of #-}

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

-- | A 'Lens' to 'Control.Lens.Getter.view'/'Control.Lens.Setter.set' the nth element 'elementOf' a 'Traversal', 'Lens' or 'Control.Lens.Iso.Iso'.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- >>> [[1],[3,4]]^.elementOf (traverse.traverse) 1
-- 3
elementOf :: Functor f => LensLike (ElementOf f) a b c c -> Int -> LensLike f a b c c
elementOf l i f a = case getElementOf (l go a) 0 of
    Found _ fb    -> fb
    Searching _ _ -> error "elementOf: index out of range"
    NotFound e    -> error $ "elementOf: " ++ e
  where
    go c = ElementOf $ \j -> if i == j then Found (j + 1) (f c) else Searching (j + 1) c

-- | Access the /nth/ element of a 'Traversable' container.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- @'element' ≡ 'elementOf' 'traverse'@
element :: Traversable t => Int -> t a :-> a
element = elementOf traverse

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | This is the trivial empty traversal.
--
-- @'ignored' :: 'Applicative' f => (c -> f d) -> a -> f a@
--
-- @'ignored' ≡ 'const' 'pure'@
ignored :: Traversal a a c d
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


------------------------------------------------------------------------------
-- Cloning Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' is completely characterized by its behavior on a 'Bazaar'.
--
-- Cloning a 'Traversal' is one way to make sure you arent given
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
-- @'cloneTraversal' :: 'LensLike' ('Bazaar' c d) a b c d -> 'Traversal' a b c d@
cloneTraversal :: Applicative f => ((c -> Bazaar c d d) -> a -> Bazaar c d b) -> (c -> f d) -> a -> f b
cloneTraversal l f = bazaar f . l sell
{-# INLINE cloneTraversal #-}

-- | A form of 'Traversal' that can be stored monomorphically in a container.
data ReifiedTraversal a b c d = ReifyTraversal { reflectTraversal :: Traversal a b c d }

-- | @type SimpleReifiedTraversal = 'Simple' 'ReifiedTraversal'@
type SimpleReifiedTraversal a b = ReifiedTraversal a a b b
