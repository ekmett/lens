{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Fold
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Fold' s a@ is a generalization of something 'Foldable'. It allows
-- you to extract multiple results from a container. A 'Foldable' container
-- can be characterized by the behavior of
-- @foldMap :: ('Foldable' t, 'Monoid' m) => (a -> m) -> t a -> m@.
-- Since we want to be able to work with monomorphic containers, we could
-- generalize this signature to @forall m. 'Monoid' m => (a -> m) -> s -> m@,
-- and then decorate it with 'Accessor' to obtain
--
-- @type 'Fold' s a = forall m. 'Monoid' m => 'Getting' m s s a a@
--
-- Every 'Getter' is a valid 'Fold' that simply doesn't use the 'Monoid'
-- it is passed.
--
-- In practice the type we use is slightly more complicated to allow for
-- better error messages and for it to be transformed by certain
-- 'Applicative' transformers.
--
-- Everything you can do with a 'Foldable' container, you can with with a 'Fold' and there are
-- combinators that generalize the usual 'Foldable' operations here.
----------------------------------------------------------------------------
module Control.Lens.Fold
  (
  -- * Folds
    Fold
  , (^?), (^..)
  -- ** Building Folds
  --, folds
  , folding
  , folded
  , unfolded
  , iterated
  , filtered
  , backwards
  , repeated
  , replicated
  , cycled
  , takingWhile
  , droppingWhile
  -- ** Folding
  , foldMapOf, foldOf
  , foldrOf, foldlOf
  , toListOf
  , anyOf, allOf
  , andOf, orOf
  , productOf, sumOf
  , traverseOf_, forOf_, sequenceAOf_
  , mapMOf_, forMOf_, sequenceOf_
  , asumOf, msumOf
  , concatMapOf, concatOf
  , elemOf, notElemOf
  , lengthOf
  , nullOf
  , headOf, lastOf
  , maximumOf, minimumOf
  , maximumByOf, minimumByOf
  , findOf
  , foldrOf', foldlOf'
  , foldr1Of, foldl1Of
  , foldrMOf, foldlMOf
  -- * Storing Folds
  , ReifiedFold(..)
  ) where

import Control.Applicative as Applicative
import Control.Applicative.Backwards
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad
import Data.Foldable as Foldable
import Data.Maybe
import Data.Monoid

-- $setup
-- >>> import Control.Lens

infixl 8 ^?, ^..

--------------------------
-- Folds
--------------------------

-- | A 'Fold' describes how to retrieve multiple values in a way that can be composed
-- with other lens-like constructions.
--
-- A @'Fold' s a@ provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other 'Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Foldable' (f a)@, then there should be a
-- @fooOf@ method that takes a @'Fold' s a@ and a value of type @s@.
--
-- A 'Getter' is a legal 'Fold' that just ignores the supplied 'Monoid'
--
-- Unlike a 'Control.Lens.Traversal.Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no lens laws that apply.
type Fold s a = forall f. (Gettable f, Applicative f) => (a -> f a) -> s -> f s


-- | Obtain a 'Fold' by lifting an operation that returns a foldable result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
folding :: (Foldable f, Applicative g, Gettable g) => (s -> f a) -> LensLike g s t a b
folding sfa agb = coerce . traverse_ agb . sfa
{-# INLINE folding #-}

-- | Obtain a 'Fold' from any 'Foldable'.
folded :: Foldable f => Fold (f a) a
folded f = coerce . getFolding . foldMap (folding# f)
{-# INLINE folded #-}

-- | Fold by repeating the input forever.
--
-- @'repeat' ≡ 'toListOf' 'repeated'@
repeated :: Fold a a
repeated f a = as where as = f a *> as

-- | A fold that replicates its input @n@ times.
--
-- @'replicate' n ≡ 'toListOf' ('replicated' n)@
replicated :: Int -> Fold a a
replicated n0 f a = go n0 where
  m = f a
  go 0 = noEffect
  go n = m *> go (n - 1)
{-# INLINE replicated #-}

-- | Transform a fold into a fold that loops over its elements over and over.
--
-- >>> take 6 $ toListOf (cycled traverse) [1,2,3]
-- [1,2,3,1,2,3]
cycled :: (Applicative f, Gettable f) => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a *> as

-- | Build a fold that unfolds its values from a seed.
--
-- @'Prelude.unfoldr' ≡ 'toListOf' . 'unfolded'@
unfolded :: (b -> Maybe (a, b)) -> Fold b a
unfolded f g b0 = go b0 where
  go b = case f b of
    Just (a, b') -> g a *> go b'
    Nothing      -> noEffect
{-# INLINE unfolded #-}

-- | @x ^. 'iterated' f@ Return an infinite fold of repeated applications of @f@ to @x@.
--
-- @'toListOf' ('iterated' f) a ≡ 'iterate' f a@
iterated :: (a -> a) -> Fold a a
iterated f g a0 = go a0 where
  go a = g a *> go (f a)
{-# INLINE iterated #-}

-- | Obtain a 'Fold' that can be composed with to filter another 'Lens', 'Control.Lens.Iso.Iso', 'Getter', 'Fold' (or 'Control.Lens.Traversal.Traversal')
--
-- Note: This is /not/ a legal 'Control.Lens.Traversal.Traversal', unless you are very careful not to invalidate the predicate on the target.
--
-- As a counter example, consider that given @evens = 'filtered' 'even'@ the second 'Control.Lens.Traversal.Traversal' law is violated:
--
-- @'over' evens 'succ' '.' 'over' evens 'succ' /= 'over' evens ('succ' '.' 'succ')@
--
-- So, in order for this to qualify as a legal 'Traversal' you can only use it for actions that preserve the result of the predicate!
--
-- @'filtered' :: (a -> 'Bool') -> 'Fold' a a@
filtered :: Applicative f => (a -> Bool) -> SimpleLensLike f a a
filtered p f a
  | p a       = f a
  | otherwise = pure a
{-# INLINE filtered #-}

-- | This allows you to traverse the elements of a 'Control.Lens.Traversal.Traversal' or 'Fold' in the opposite order.
-- This will demote an 'Control.Lens.IndexedTraversal.IndexedTraversal' or 'Control.Lens.IndexedFold.IndexedFold' to a regular 'Control.Lens.Traversal.Traversal' or 'Fold';
-- to preserve the indices, use 'Control.Lens.IndexedFold.ibackwards' instead.
--
-- Note: 'backwards' should have no impact on a 'Getter', 'Control.Lens.Setter.Setter', 'Lens' or 'Control.Lens.Iso.Iso'.
--
-- To change the direction of an 'Control.Lens.Iso.Iso', use 'Control.Lens.Isomorphic.from'.
backwards :: LensLike (Backwards f) s t a b -> LensLike f s t a b
backwards l f = forwards# $ l (backwards# f)
{-# INLINE backwards #-}

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Control.Lens.Iso.Iso', 'Getter' or 'Control.Lens.Traversal.Traversal' while a predicate holds.
--
-- @'takeWhile' p ≡ 'toListOf' ('takingWhile' p 'folded')@
--
-- >>> toListOf (takingWhile (<=3) folded) [1..]
-- [1,2,3]
takingWhile :: (Gettable f, Applicative f)
            => (a -> Bool)
            -> Getting (Endo (f s)) s s a a
            -> LensLike f s s a a
takingWhile p l f = foldrOf l (\a r -> if p a then f a *> r else noEffect) noEffect
{-# INLINE takingWhile #-}


-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Control.Lens.Iso.Iso', 'Getter' or 'Control.Lens.Traversal.Traversal' while a predicate holds.
--
-- @'dropWhile' p ≡ 'toListOf' ('droppingWhile' p 'folded')@
--
-- >>> toListOf (droppingWhile (<=3) folded) [1..6]
-- [4,5,6]
--
-- >>> toListOf (droppingWhile (<=3) folded) [1,6,1]
-- [6,1]
droppingWhile :: (Gettable f, Applicative f)
              => (a -> Bool)
              -> Getting (Endo (f s, f s)) s s a a
              -> LensLike f s s a a
droppingWhile p l f = fst . foldrOf l (\a r -> let s = f a *> snd r in if p a then (fst r, s) else (s, s)) (noEffect, noEffect)
{-# INLINE droppingWhile #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- |
-- @'Data.Foldable.foldMap' = 'foldMapOf' 'folded'@
--
-- @'foldMapOf' ≡ 'views'@
--
-- @
-- 'foldMapOf' ::             'Getter' s a           -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r => 'Fold' s a             -> (a -> r) -> s -> r
-- 'foldMapOf' ::             'Simple' 'Lens' s a      -> (a -> r) -> s -> r
-- 'foldMapOf' ::             'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> r) -> s -> r
-- @
foldMapOf :: Getting r s t a b -> (a -> r) -> s -> r
foldMapOf l f = runAccessor# (l (accessor# f))
{-# INLINE foldMapOf #-}

-- |
-- @'Data.Foldable.fold' = 'foldOf' 'folded'@
--
-- @'foldOf' ≡ 'view'@
--
-- @
-- 'foldOf' ::             'Getter' s m           -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Fold' s m             -> s -> m
-- 'foldOf' ::             'Simple' 'Lens' s m      -> s -> m
-- 'foldOf' ::             'Simple' 'Control.Lens.Iso.Iso' s m       -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Simple' 'Control.Lens.Traversal.Traversal' s m -> s -> m
-- @
foldOf :: Getting a s t a b -> s -> a
foldOf l = runAccessor# (l Accessor)
{-# INLINE foldOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Control.Lens.Traversal.Traversal'.
--
-- @'Data.Foldable.foldr' ≡ 'foldrOf' 'folded'@
--
-- @
-- 'foldrOf' :: 'Getter' s a           -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Fold' s a             -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Simple' 'Lens' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf :: Getting (Endo r) s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf l f z t = appEndo (foldMapOf l (endo# f) t) z
{-# INLINE foldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Control.Lens.Traversal.Traversal'.
--
-- @'Data.Foldable.foldl' ≡ 'foldlOf' 'folded'@
--
-- @
-- 'foldlOf' :: 'Getter' s a           -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Fold' s a             -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Simple' 'Lens' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf :: Getting (Dual (Endo r)) s t a b -> (r -> a -> r) -> r -> s -> r
foldlOf l f z t = appEndo (getDual (foldMapOf l (dual# (endo# (flip f))) t)) z
{-# INLINE foldlOf #-}

-- | Extract a list of the targets of a 'Fold'. See also ('^..').
--
-- @
-- 'Data.Foldable.toList' ≡ 'toListOf' 'folded'
-- ('^..') ≡ 'flip' 'toListOf'
-- @

-- >>> toListOf both ("hello","world")
-- ["hello","world"]
--
-- @
-- 'toListOf' :: 'Getter' s a           -> s -> [a]
-- 'toListOf' :: 'Fold' s a             -> s -> [a]
-- 'toListOf' :: 'Simple' 'Lens' s a      -> s -> [a]
-- 'toListOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> [a]
-- 'toListOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> [a]
-- @
toListOf :: Getting [a] s t a b -> s -> [a]
toListOf l = foldMapOf l return
{-# INLINE toListOf #-}

-- |
--
-- A convenient infix (flipped) version of 'toListOf'.
--
-- >>> [[1,2],[3]]^..traverse.traverse
-- [1,2,3]
--
-- >>> (1,2)^..both
-- [1,2]
--
-- @
-- 'Data.Foldable.toList' xs ≡ xs '^..' 'folded'
-- ('^..') ≡ 'flip' 'toListOf'
-- @
--
-- @
-- ('^..') :: s -> 'Getter' s a           -> [a]
-- ('^..') :: s -> 'Fold' s a             -> [a]
-- ('^..') :: s -> 'Simple' 'Lens' s a      -> [a]
-- ('^..') :: s -> 'Simple' 'Control.Lens.Iso.Iso' s a       -> [a]
-- ('^..') :: s -> 'Simple' 'Control.Lens.Traversal.Traversal' s a -> [a]
-- @
(^..) :: s -> Getting [a] s t a b -> [a]
s ^.. l = foldMapOf l return s

-- | Returns 'True' if every target of a 'Fold' is 'True'.
--
-- >>> andOf both (True,False)
-- False
-- >>> andOf both (True,True)
-- True
--
-- @'Data.Foldable.and' ≡ 'andOf' 'folded'@
--
-- @
-- 'andOf' :: 'Getter' s 'Bool'           -> s -> 'Bool'
-- 'andOf' :: 'Fold' s 'Bool'             -> s -> 'Bool'
-- 'andOf' :: 'Simple' 'Lens' s 'Bool'      -> s -> 'Bool'
-- 'andOf' :: 'Simple' 'Control.Lens.Iso.Iso' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s 'Bool' -> s -> 'Bool'
-- @
andOf :: Getting All s t Bool b -> s -> Bool
andOf l = getAll# (foldMapOf l All)
{-# INLINE andOf #-}

-- | Returns 'True' if any target of a 'Fold' is 'True'.
--
-- >>> orOf both (True,False)
-- True
-- >>> orOf both (False,False)
-- False
--
-- @'Data.Foldable.or' ≡ 'orOf' 'folded'@
--
-- @
-- 'orOf' :: 'Getter' s 'Bool'           -> s -> 'Bool'
-- 'orOf' :: 'Fold' s 'Bool'             -> s -> 'Bool'
-- 'orOf' :: 'Simple' 'Lens' s 'Bool'      -> s -> 'Bool'
-- 'orOf' :: 'Simple' 'Control.Lens.Iso.Iso' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s 'Bool' -> s -> 'Bool'
-- @
orOf :: Getting Any s t Bool b -> s -> Bool
orOf l = getAny# (foldMapOf l Any)
{-# INLINE orOf #-}

-- | Returns 'True' if any target of a 'Fold' satisfies a predicate.
--
-- >>> anyOf both (=='x') ('x','y')
-- True
-- >>> import Data.Data.Lens
-- >>> anyOf biplate (== "world") (((),2::Int),"hello",("world",11))
-- True
--
-- @'Data.Foldable.any' ≡ 'anyOf' 'folded'@
--
-- @
-- 'anyOf' :: 'Getter' s a               -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Fold' s a                 -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Simple' 'Lens' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> 'Bool') -> s -> 'Bool'
-- @
anyOf :: Getting Any s t a b -> (a -> Bool) -> s -> Bool
anyOf l f = getAny# $ foldMapOf l (any# f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold' satisfies a predicate.
--
-- >>> allOf both (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @'Data.Foldable.all' ≡ 'allOf' 'folded'@
--
-- @
-- 'allOf' :: 'Getter' s a           -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Fold' s a             -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Simple' 'Lens' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> 'Bool') -> s -> 'Bool'
-- @
allOf :: Getting All s t a b -> (a -> Bool) -> s -> Bool
allOf l f = getAll# $ foldMapOf l (all# f)
{-# INLINE allOf #-}

-- | Calculate the product of every number targeted by a 'Fold'
--
-- >>> productOf both (4,5)
-- 20
-- >>> productOf folded [1,2,3,4,5]
-- 120
--
-- @'Data.Foldable.product' ≡ 'productOf' 'folded'@
--
-- @
-- 'productOf' ::          'Getter' s a           -> s -> a
-- 'productOf' :: 'Num' a => 'Fold' s a             -> s -> a
-- 'productOf' ::          'Simple' 'Lens' s a      -> s -> a
-- 'productOf' ::          'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> a
-- @
productOf :: Getting (Product a) s t a b -> s -> a
productOf l = getProduct# $ foldMapOf l Product
{-# INLINE productOf #-}

-- | Calculate the sum of every number targeted by a 'Fold'.
--
-- >>> sumOf both (5,6)
-- 11
-- >>> sumOf folded [1,2,3,4]
-- 10
-- >>> sumOf (folded.both) [(1,2),(3,4)]
-- 10
-- >>> import Data.Data.Lens
-- >>> sumOf biplate [(1::Int,[]),(2,[(3::Int,4::Int)])] :: Int
-- 10
--
-- @'Data.Foldable.sum' ≡ 'sumOf' 'folded'@
--
-- @
-- 'sumOf' '_1' :: (a, b) -> a
-- 'sumOf' ('folded' . '_1') :: ('Foldable' f, 'Num' a) => f (a, b) -> a
-- @
--
-- @
-- 'sumOf' ::          'Getter' s a           -> s -> a
-- 'sumOf' :: 'Num' a => 'Fold' s a             -> s -> a
-- 'sumOf' ::          'Simple' 'Lens' s a      -> s -> a
-- 'sumOf' ::          'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> a
-- @
sumOf :: Getting (Sum a) s t a b -> s -> a
sumOf l = getSum# $ foldMapOf l Sum
{-# INLINE sumOf #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor') -based answer,
-- but unlike 'Control.Lens.Traversal.traverseOf' do not construct a new structure. 'traverseOf_' generalizes
-- 'Data.Foldable.traverse_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'traverseOf_' can work over any 'Functor', but when passed a 'Fold', 'traverseOf_' requires
-- an 'Applicative'.
--
-- >>> traverseOf_ both putStrLn ("hello","world")
-- hello
-- world
--
-- @'Data.Foldable.traverse_' ≡ 'traverseOf_' 'folded'@
--
-- @
-- 'traverseOf_' '_2' :: 'Functor' f => (c -> f r) -> (d, c) -> f ()
-- 'traverseOf_' 'Data.Either.Lens.traverseLeft' :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ()
-- @
--
-- The rather specific signature of 'traverseOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'traverseOf_' :: 'Functor' f     => 'Getter' s a           -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Fold' s a             -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Simple' 'Lens' s a      -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> f r) -> s -> f ()
-- @
traverseOf_ :: Functor f => Getting (Traversed f) s t a b -> (a -> f r) -> s -> f ()
traverseOf_ l f = getTraversed# (foldMapOf l (traversed# (void . f)))
{-# INLINE traverseOf_ #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor') -based answer,
-- but unlike 'Control.Lens.Traversal.forOf' do not construct a new structure. 'forOf_' generalizes
-- 'Data.Foldable.for_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'forOf_' can work over any 'Functor', but when passed a 'Fold', 'forOf_' requires
-- an 'Applicative'.
--
-- @'for_' ≡ 'forOf_' 'folded'@
--
-- The rather specific signature of 'forOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'forOf_' :: 'Functor' f     => 'Getter' s a           -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Fold' s a             -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Simple' 'Lens' s a      -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> (a -> f r) -> f ()
-- @
forOf_ :: Functor f => Getting (Traversed f) s t a b -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in observed by a 'Fold' on a structure from left to right, ignoring the results.
--
-- @'sequenceA_' ≡ 'sequenceAOf_' 'folded'@
--
-- @
-- 'sequenceAOf_' :: 'Functor' f     => 'Getter' s (f ())           -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Fold' s (f ())             -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Simple' 'Lens' s (f ())      -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Simple' 'Iso' s (f ())       -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Simple' 'Control.Lens.Traversal.Traversal' s (f ()) -> s -> f ()
-- @
sequenceAOf_ :: Functor f => Getting (Traversed f) s t (f ()) b -> s -> f ()
sequenceAOf_ l = getTraversed# (foldMapOf l (traversed# void))
{-# INLINE sequenceAOf_ #-}

-- | Map each target of a 'Fold' on a structure to a monadic action, evaluate these actions from left to right, and ignore the results.
--
-- @'Data.Foldable.mapM_' ≡ 'mapMOf_' 'folded'@
--
-- @
-- 'mapMOf_' :: 'Monad' m => 'Getter' s a           -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Fold' s a             -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Simple' 'Lens' s a      -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> m r) -> s -> m ()
-- @
mapMOf_ :: Monad m => Getting (Sequenced m) s t a b -> (a -> m r) -> s -> m ()
mapMOf_ l f = getSequenced# (foldMapOf l (sequenced# (liftM skip . f)))
{-# INLINE mapMOf_ #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

-- | 'forMOf_' is 'mapMOf_' with two of its arguments flipped.
--
-- @'Data.Foldable.forM_' ≡ 'forMOf_' 'folded'@
--
-- @
-- 'forMOf_' :: 'Monad' m => 'Getter' s a           -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Fold' s a             -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Simple' 'Lens' s a      -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> (a -> m r) -> m ()
-- @
forMOf_ :: Monad m => Getting (Sequenced m) s t a b -> s -> (a -> m r) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- | Evaluate each monadic action referenced by a 'Fold' on the structure from left to right, and ignore the results.
--
-- @'Data.Foldable.sequence_' ≡ 'sequenceOf_' 'folded'@
--
-- @
-- 'sequenceOf_' :: 'Monad' m => 'Getter' s (m a)           -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Fold' s (m a)             -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Simple' 'Lens' s (m a)      -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' s (m a) -> s -> m ()
-- @
sequenceOf_ :: Monad m => Getting (Sequenced m) s t (m a) b -> s -> m ()
sequenceOf_ l = getSequenced# (foldMapOf l (sequenced# (liftM skip)))
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- @'asum' ≡ 'asumOf' 'folded'@
--
-- @
-- 'asumOf' :: 'Alternative' f => 'Getter' s a           -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Fold' s a             -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Simple' 'Lens' s a      -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> f a
-- @
asumOf :: Alternative f => Getting (Endo (f a)) s t (f a) b -> s -> f a
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- @'msum' ≡ 'msumOf' 'folded'@
--
-- @
-- 'msumOf' :: 'MonadPlus' m => 'Getter' s a           -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Fold' s a             -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Simple' 'Lens' s a      -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> m a
-- @
msumOf :: MonadPlus m => Getting (Endo (m a)) s t (m a) b -> s -> m a
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- | Does the element occur anywhere within a given 'Fold' of the structure?
--
-- >>> elemOf both "hello" ("hello","world")
-- True
--
-- @'elem' ≡ 'elemOf' 'folded'@
--
-- @
-- 'elemOf' :: 'Eq' a => 'Getter' s a           -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Fold' s a             -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Simple' 'Lens' s a      -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Simple' 'Control.Lens.Iso.Iso' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> 'Bool'
-- @
elemOf :: Eq a => Getting Any s t a b -> a -> s -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- @'notElem' ≡ 'notElemOf' 'folded'@
--
-- @
-- 'notElemOf' :: 'Eq' a => 'Getter' s a           -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Fold' s a             -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Simple' 'Control.Lens.Iso.Iso' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Simple' 'Lens' s a      -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> 'Bool'
-- @
notElemOf :: Eq a => Getting All s t a b -> a -> s -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- | Map a function over all the targets of a 'Fold' of a container and concatenate the resulting lists.
--
-- @'concatMap' ≡ 'concatMapOf' 'folded'@
--
-- @
-- 'concatMapOf' :: 'Getter' s a           -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Fold' s a             -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Simple' 'Lens' s a      -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> [r]) -> s -> [r]
-- @
concatMapOf :: Getting [r] s t a b -> (a -> [r]) -> s -> [r]
concatMapOf l ces = runAccessor# (l (accessor# ces))
{-# INLINE concatMapOf #-}

-- | Concatenate all of the lists targeted by a 'Fold' into a longer list.
--
-- >>> concatOf both ("pan","ama")
-- "panama"
--
-- @
-- 'concat' ≡ 'concatOf' 'folded'
-- 'concatOf' ≡ 'view'
-- @
--
-- @
-- 'concatOf' :: 'Getter' s [r]           -> s -> [r]
-- 'concatOf' :: 'Fold' s [r]             -> s -> [r]
-- 'concatOf' :: 'Simple' 'Control.Lens.Iso.Iso' s [r]       -> s -> [r]
-- 'concatOf' :: 'Simple' 'Lens' s [r]      -> s -> [r]
-- 'concatOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s [r] -> s -> [r]
-- @
concatOf :: Getting [r] s t [r] b -> s -> [r]
concatOf = view
{-# INLINE concatOf #-}

-- |
-- Note: this can be rather inefficient for large containers.
--
-- @'length' ≡ 'lengthOf' 'folded'@
--
-- >>> lengthOf _1 ("hello",())
-- 1
--
-- @'lengthOf' ('folded' . 'folded') :: 'Foldable' f => f (g a) -> 'Int'@
--
-- @
-- 'lengthOf' :: 'Getter' s a           -> s -> 'Int'
-- 'lengthOf' :: 'Fold' s a             -> s -> 'Int'
-- 'lengthOf' :: 'Simple' 'Lens' s a      -> s -> 'Int'
-- 'lengthOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'Int'
-- @
lengthOf :: Getting (Sum Int) s t a b -> s -> Int
lengthOf l = getSum# (foldMapOf l (\_ -> Sum 1))
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Control.Lens.Traversal.Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'. See also ('^?').
--
-- @'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'headOf' 'folded'@
--
-- @
-- 'headOf' :: 'Getter' s a           -> s -> 'Maybe' a
-- 'headOf' :: 'Fold' s a             -> s -> 'Maybe' a
-- 'headOf' :: 'Simple' 'Lens' s a      -> s -> 'Maybe' a
-- 'headOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'Maybe' a
-- 'headOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'Maybe' a
-- @
headOf :: Getting (First a) s t a b -> s -> Maybe a
headOf l = getFirst# (foldMapOf l (first# Just))
{-# INLINE headOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Control.Lens.Traversal.Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- When using a 'Control.Lens.Traversal.Traversal' as a partial 'Control.Lens.Type.Lens', or a 'Fold' as a partial 'Getter' this can be a convenient
-- way to extract the optional value.
--
-- @('^?') ≡ 'flip' 'headOf'@
--
-- @
-- ('^?') :: s -> 'Getter' s a           -> 'Maybe' a
-- ('^?') :: s -> 'Fold' s a             -> 'Maybe' a
-- ('^?') :: s -> 'Simple' 'Lens' s a      -> 'Maybe' a
-- ('^?') :: s -> 'Simple' 'Control.Lens.Iso.Iso' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Simple' 'Control.Lens.Traversal.Traversal' s a -> 'Maybe' a
-- @
(^?) :: s -> Getting (First a) s t a b -> Maybe a
a ^? l = getFirst (foldMapOf l (first# Just) a)
{-# INLINE (^?) #-}

-- | Perform a safe 'last' of a 'Fold' or 'Control.Lens.Traversal.Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- @
-- 'lastOf' :: 'Getter' s a           -> s -> 'Maybe' a
-- 'lastOf' :: 'Fold' s a             -> s -> 'Maybe' a
-- 'lastOf' :: 'Simple' 'Lens' s a      -> s -> 'Maybe' a
-- 'lastOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'Maybe' a
-- @
lastOf :: Getting (Last a) s t a b -> s -> Maybe a
lastOf l = getLast# (foldMapOf l (last# Just))
{-# INLINE lastOf #-}

-- |
-- Returns 'True' if this 'Fold' or 'Control.Lens.Traversal.Traversal' has no targets in the given container.
--
-- Note: 'nullOf' on a valid 'Control.Lens.Iso.Iso', 'Lens' or 'Getter' should always return 'False'
--
-- @'null' ≡ 'nullOf' 'folded'@
--
-- This may be rather inefficient compared to the 'null' check of many containers.
--
-- >>> nullOf _1 (1,2)
-- False
--
-- @'nullOf' ('folded' '.' '_1' '.' 'folded') :: 'Foldable' f => f (g a, b) -> 'Bool'@
--
-- @
-- 'nullOf' :: 'Getter' s a           -> s -> 'Bool'
-- 'nullOf' :: 'Fold' s a             -> s -> 'Bool'
-- 'nullOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Simple' 'Lens' s a      -> s -> 'Bool'
-- 'nullOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'Bool'
-- @
nullOf :: Getting All s t a b -> s -> Bool
nullOf l = getAll# (foldMapOf l (\_ -> All False))
{-# INLINE nullOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold' or 'Control.Lens.Traversal.Traversal'
--
-- Note: maximumOf on a valid 'Control.Lens.Iso.Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- @'maximum' ≡ 'fromMaybe' ('error' "empty") '.' 'maximumOf' 'folded'@
--
-- @
-- 'maximumOf' ::          'Getter' s a           -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Fold' s a             -> s -> 'Maybe' a
-- 'maximumOf' ::          'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'Maybe' a
-- 'maximumOf' ::          'Simple' 'Lens' s a      -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'Maybe' a
-- @
maximumOf :: Getting (Max a) s t a b -> s -> Maybe a
maximumOf l = getMax . foldMapOf l Max
{-# INLINE maximumOf #-}

-- |
-- Obtain the minimum element (if any) targeted by a 'Fold' or 'Control.Lens.Traversal.Traversal'
--
-- Note: minimumOf on a valid 'Control.Lens.Iso.Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- @'minimum' ≡ 'Data.Maybe.fromMaybe' ('error' "empty") '.' 'minimumOf' 'folded'@
--
-- @
-- 'minimumOf' ::          'Getter' s a           -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Fold' s a             -> s -> 'Maybe' a
-- 'minimumOf' ::          'Simple' 'Control.Lens.Iso.Iso' s a       -> s -> 'Maybe' a
-- 'minimumOf' ::          'Simple' 'Lens' s a      -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> s -> 'Maybe' a
-- @
minimumOf :: Getting (Min a) s t a b -> s -> Maybe a
minimumOf l = getMin . foldMapOf l Min
{-# INLINE minimumOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold', 'Control.Lens.Traversal.Traversal', 'Lens', 'Control.Lens.Iso.Iso',
-- or 'Getter' according to a user supplied ordering.
--
-- @'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' "empty") '.' 'maximumByOf' 'folded' cmp@
--
-- @
-- 'maximumByOf' :: 'Getter' s a           -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Fold' s a             -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Simple' 'Lens' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
maximumByOf :: Getting (Endo (Maybe a)) s t a b -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then a else b)
{-# INLINE maximumByOf #-}

-- |
-- Obtain the minimum element (if any) targeted by a 'Fold', 'Control.Lens.Traversal.Traversal', 'Lens', 'Control.Lens.Iso.Iso'
-- or 'Getter' according to a user supplied ordering.
--
-- @'minimumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' "empty") '.' 'minimumByOf' 'folded' cmp@
--
-- @
-- 'minimumByOf' :: 'Getter' s a           -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Fold' s a             -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Simple' 'Lens' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
minimumByOf :: Getting (Endo (Maybe a)) s t a b -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then b else a)
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Lens' (or 'Control.Lens.Getter.Getter', 'Control.Lens.Iso.Iso', 'Control.Lens.Fold.Fold', or 'Control.Lens.Traversal.Traversal'),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- @
-- 'findOf' :: 'Getter' s a           -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Fold' s a             -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Simple' 'Lens' s a      -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- @
findOf :: Getting (First a) s t a b -> (a -> Bool) -> s -> Maybe a
findOf l p = getFirst# (foldMapOf l step) where
  step a
    | p a       = First (Just a)
    | otherwise = First Nothing
{-# INLINE findOf #-}

-- |
-- A variant of 'foldrOf' that has no base case and thus may only be applied
-- to lenses and structures such that the lens views at least one element of
-- the structure.
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- 'Data.Foldable.foldr1' ≡ 'foldr1Of' 'folded'
-- @
--
-- @
-- 'foldr1Of' :: 'Getter' s a           -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Fold' s a             -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Simple' 'Lens' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of :: Getting (Endo (Maybe a)) s t a b -> (a -> a -> a) -> s -> a
foldr1Of l f xs = fromMaybe (error "foldr1Of: empty structure")
                            (foldrOf l mf Nothing xs) where
  mf x Nothing = Just x
  mf x (Just y) = Just (f x y)
{-# INLINE foldr1Of #-}

-- | A variant of 'foldlOf' that has no base case and thus may only be applied to lenses and strutures such
-- that the lens views at least one element of the structure.
--
-- @
-- 'foldl1Of' l f ≡ 'Prelude.foldl1Of' l f . 'toList'
-- 'Data.Foldable.foldl1' ≡ 'foldl1Of' 'folded'
-- @
--
-- @
-- 'foldl1Of' :: 'Getter' s a           -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Fold' s a             -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Simple' 'Lens' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of :: Getting (Dual (Endo (Maybe a))) s t a b -> (a -> a -> a) -> s -> a
foldl1Of l f xs = fromMaybe (error "foldl1Of: empty structure") (foldlOf l mf Nothing xs) where
  mf Nothing y = Just y
  mf (Just x) y = Just (f x y)
{-# INLINE foldl1Of #-}

-- | Strictly fold right over the elements of a structure.
--
-- @'Data.Foldable.foldr'' ≡ 'foldrOf'' 'folded'@
--
-- @
-- 'foldrOf'' :: 'Getter' s a           -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Fold' s a             -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Simple' 'Lens' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf' :: Getting (Dual (Endo (r -> r))) s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf' l f z0 xs = foldlOf l f' id xs z0
  where f' k x z = k $! f x z
{-# INLINE foldrOf' #-}

-- | Fold over the elements of a structure, associating to the left, but strictly.
--
-- @'Data.Foldable.foldl'' ≡ 'foldlOf'' 'folded'@
--
-- @
-- 'foldlOf'' :: 'Getter' s a           -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Fold' s a             -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Simple' 'Lens' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf' :: Getting (Endo (r -> r)) s t a b -> (r -> a -> r) -> r -> s -> r
foldlOf' l f z0 xs = foldrOf l f' id xs z0
  where f' x k z = k $! f z x
{-# INLINE foldlOf' #-}

-- | Monadic fold over the elements of a structure, associating to the right,
-- i.e. from right to left.
--
-- @'Data.Foldable.foldrM' ≡ 'foldrMOf' 'folded'@
--
-- @
-- 'foldrMOf' :: 'Monad' m => 'Getter' s a           -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Fold' s a             -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Simple' 'Lens' s a      -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> r -> m r) -> r -> s -> m r
-- @
foldrMOf :: Monad m
         => Getting (Dual (Endo (r -> m r))) s t a b
         -> (a -> r -> m r) -> r -> s -> m r
foldrMOf l f z0 xs = foldlOf l f' return xs z0
  where f' k x z = f x z >>= k
{-# INLINE foldrMOf #-}

-- | Monadic fold over the elements of a structure, associating to the left,
-- i.e. from left to right.
--
-- @'Data.Foldable.foldlM' ≡ 'foldlMOf' 'folded'@
--
-- @
-- 'foldlMOf' :: 'Monad' m => 'Getter' s a           -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Fold' s a             -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Simple' 'Lens' s a      -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' s a -> (r -> a -> m r) -> r -> s -> m r
-- @
foldlMOf :: Monad m
         => Getting (Endo (r -> m r)) s t a b
         -> (r -> a -> m r) -> r -> s -> m r
foldlMOf l f z0 xs = foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}

-- | Useful for storing folds in containers.
newtype ReifiedFold s a = ReifyFold { reflectFold :: Fold s a }
