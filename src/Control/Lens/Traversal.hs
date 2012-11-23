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

  -- ** Lensing Traversals
  , element, elementOf

  -- * Traversing and Lensing
  , traverseOf, forOf, sequenceAOf
  , mapMOf, forMOf, sequenceOf
  , transposeOf
  , mapAccumLOf, mapAccumROf
  , scanr1Of, scanl1Of

  -- * Parts and Holes
  , partsOf
  , partsOf'
  , unsafePartsOf
  , unsafePartsOf'
  , holesOf

  -- * Common Traversals
  , Traversable(traverse)
  , traverseLeft
  , traverseRight
  , both
  , beside
  , taking
  , dropping

  -- * Cloning Traversals
  , cloneTraversal
  , ReifiedTraversal(..)

  -- * Simple
  , SimpleTraversal
  , SimpleReifiedTraversal

  -- * Exposed Implementation Details
  , Bazaar(..)
  ) where

import Control.Applicative              as Applicative
import Control.Applicative.Backwards
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Internal.BazaarT
import Control.Lens.Internal.Combinators
import Control.Lens.Type
import Control.Monad.State.Class        as State
import Control.Monad.Trans.State.Lazy   as Lazy
import Data.Maybe
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

-------------------------------------------------------------------------------
-- Parts and Holes
-------------------------------------------------------------------------------

-- | 'partsOf' turns a 'Traversal' into a 'Lens' that resembles an early version of the @uniplate@ (or @biplate@) type.
--
-- /Note:/ You should really try to maintain the invariant of the number of children in the list.
--
-- Any extras will be lost. If you do not supply enough, then the remainder will come from the original structure.
--
-- So technically, this is only a lens if you do not change the number of results it returns.
--
-- When applied to a 'Fold' the result is merely a 'Getter'.
--
-- @
-- 'partsOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> 'Simple' 'Lens' s [a]
-- 'partsOf' :: 'Simple' 'Lens' s a      -> 'Simple' 'Lens' s [a]
-- 'partsOf' :: 'Simple' 'Traversal' s a -> 'Simple' 'Lens' s [a]
-- 'partsOf' :: 'Fold' s a             -> 'Getter' s [a]
-- 'partsOf' :: 'Getter' s a           -> 'Getter' s [a]
-- @
partsOf :: Functor f => LensLike (BazaarT a a f) s t a a -> LensLike f s t [a] [a]
partsOf l f s = outsT b <$> f (insT b) where b = l sellT s
{-# INLINE partsOf #-}

-- | A type-restricted version of 'partsOf' that can only be used with a 'Traversal'.
partsOf' :: LensLike (Bazaar a a) s t a a -> Lens s t [a] [a]
partsOf' l f s = outs b <$> f (ins b) where b = l sell s
{-# INLINE partsOf' #-}

-- | 'unsafePartsOf' turns a 'Traversal' into a @uniplate@ (or @biplate@) family.
--
-- If you do not need the types of @s@ and @t@ to be different, it is recommended that
-- you use 'partsOf'
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
-- 'unsafePartsOf' :: 'Control.Lens.Iso.Iso' s t a b       -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Lens' s t a b      -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Traversal' s t a b -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Fold' s a          -> 'Getter' s [a]
-- 'unsafePartsOf' :: 'Getter' s a        -> 'Getter' s [a]
-- @
unsafePartsOf :: Functor f => LensLike (BazaarT a b f) s t a b -> LensLike f s t [a] [b]
unsafePartsOf l f s = unsafeOutsT b <$> f (insT b) where b = l sellT s
{-# INLINE unsafePartsOf #-}

unsafePartsOf' :: LensLike (Bazaar a b) s t a b -> Lens s t [a] [b]
unsafePartsOf' l f s = unsafeOuts b <$> f (ins b) where b = l sell s
{-# INLINE unsafePartsOf' #-}

-- | The one-level version of 'contextsOf'. This extracts a list of the immediate children according to a given 'Traversal' as editable contexts.
--
-- Given a context you can use 'pos' to see the values, 'peek' at what the structure would be like with an edited result, or simply 'extract' the original structure.
--
-- @
-- propChildren l x = 'childrenOf' l x '==' 'map' 'pos' ('holesOf' l x)
-- propId l x = 'all' ('==' x) [extract w | w <- 'holesOf' l x]
-- @
--
-- @
-- 'holesOf' :: 'Simple' 'Iso' s a       -> s -> ['Context' a a s]
-- 'holesOf' :: 'Simple' 'Lens' s a      -> s -> ['Context' a a s]
-- 'holesOf' :: 'Simple' 'Traversal' s a -> s -> ['Context' a a s]
-- @
holesOf :: LensLike (Bazaar a a) s t a a -> s -> [Context a a t]
holesOf l a = f (ins b) (outs b) where
  b = l sell a
  f []     _ = []
  f (x:xs) g = Context (g . (:xs)) x : f xs (g . (x:))
{-# INLINE holesOf #-}

-- | A 'Lens' to 'Control.Lens.Getter.view'/'Control.Lens.Setter.set' the nth element 'elementOf' a 'Traversal', 'Lens' or 'Control.Lens.Iso.Iso'.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error. This also works transparently
-- with Folds, returning a getter.
--
-- >>> [[1],[3,4]] & elementOf (traverse.traverse) 1 .~ 5
-- [[1],[5,4]]
--
-- >>> [[1],[3,4]]^.elementOf (folded.folded) 1
-- 3
--
-- >>> [0..]^.elementOf folded 5
-- 5
--
-- >>> take 10 $ (elementOf traverse 3 .~ 16) [0..]
-- [0,1,2,16,4,5,6,7,8,9]
elementOf :: Functor f => LensLike (ElementOf f) s t a a -> Int -> LensLike f s t a a
elementOf l i f s = case getElementOf (l go s) 0 of
    Searching _ _ mft -> fromMaybe (error "elOf: index out of range") mft
  where
    go a = ElementOf $ \j -> Searching (j + 1) a (if i == j then Just (f a) else Nothing)

-- | Access the /nth/ element of a 'Traversable' container.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- @'element' ≡ 'elementOf' 'traverse'@
element :: Traversable t => Int -> Simple Lens (t a) a
element = elementOf traverse

------------------------------------------------------------------------------
-- Internal functions used by 'partsOf', 'holesOf', etc.
------------------------------------------------------------------------------
ins :: Bazaar a b t -> [a]
ins = toListOf bazaar
{-# INLINE ins #-}

outs :: Bazaar a a t -> [a] -> t
outs = evalState . bazaar (\oldVal -> State.state (unconsWithDefault oldVal))
{-# INLINE outs #-}

unsafeOuts :: Bazaar a b t -> [b] -> t
unsafeOuts = evalState . bazaar (\_ -> State.state (unconsWithDefault fakeVal))
  where fakeVal = error "unsafePartsOf': not enough elements were supplied"
{-# INLINE unsafeOuts #-}

insT :: BazaarT a b f t -> [a]
insT = toListOf bazaarT
{-# INLINE insT #-}

outsT :: BazaarT a a f t -> [a] -> t
outsT = evalState . bazaarT (\oldVal -> State.state (unconsWithDefault oldVal))
{-# INLINE outsT #-}

unsafeOutsT :: BazaarT a b f t -> [b] -> t
unsafeOutsT = evalState . bazaarT (\_ -> State.state (unconsWithDefault fakeVal))
  where fakeVal = error "unsafePartsOf: not enough elements were supplied"
{-# INLINE unsafeOutsT #-}

unconsWithDefault :: a -> [a] -> (a,[a])
unconsWithDefault d []     = (d,[])
unconsWithDefault _ (x:xs) = (x,xs)
{-# INLINE unconsWithDefault #-}

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | Traverse both parts of a tuple with matching types.
--
-- >>> both *~ 10 $ (1,2)
-- (10,20)
-- >>> over both length ("hello","world")
-- (5,5)
-- >>> ("hello","world")^.both
-- "helloworld"
both :: Traversal (a,a) (b,b) a b
both f ~(a,a') = (,) <$> f a <*> f a'
{-# INLINE both #-}

-- | Apply a different 'Traversal' or 'Control.Lens.Fold.Fold' to each side of a tuple.
--
-- >>> ("hello",["world","!!!"])^..beside id traverse
-- ["hello","world","!!!"]
beside :: Applicative f => LensLike f s t a b -> LensLike f s' t' a b -> LensLike f (s,s') (t,t') a b
beside l r f ~(s,s') = (,) <$> l f s <*> r f s'
{-# INLINE beside #-}

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
--
-- >>> [("hello","world"),("!!!","!!!")]^.. taking 2 (traverse.both)
-- ["hello","world"]
--
-- >>> [1..]^.. taking 3 traverse
-- [1,2,3]
--
-- >>> over (taking 5 traverse) succ "hello world"
-- "ifmmp world"
taking :: Applicative f => Int -> SimpleLensLike (BazaarT a a f) s a -> SimpleLensLike f s a
taking n l f s = outsT b <$> traverse f (take n $ insT b) where b = l sellT s
{-# INLINE taking #-}

-- | Visit all but the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
--
-- >>> ("hello","world") ^? dropping 1 both
-- Just "world"
--
-- Dropping works on infinite traversals as well.
--
-- >>> [1..]^? dropping 1 folded
-- Just 2
dropping :: Applicative f => Int -> SimpleLensLike (Indexing f) s a -> SimpleLensLike f s a
dropping n l f s = case runIndexing (l (\a -> Indexing $ \i -> (if i >= n then f a else pure a, i + 1)) s) 0 of
  (r, _) -> r
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
