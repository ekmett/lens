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
-- A @'Fold' a c@ is a generalization of something 'Foldable'. It allows
-- you to extract multiple results from a container. A 'Foldable' container
-- can be characterized by the behavior of
-- @foldMap :: ('Foldable' t, 'Monoid' m) => (c -> m) -> t c -> m@.
-- Since we want to be able to work with monomorphic containers, we could
-- generalize this signature to @forall m. 'Monoid' m => (c -> m) -> a -> m@,
-- and then decorate it with 'Accessor' to obtain
--
-- @type 'Fold' a c = forall m. 'Monoid' m => 'Getting' m a a c c@
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
-- A @'Fold' a c@ provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other 'Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Foldable' (f c)@, then there should be a
-- @fooOf@ method that takes a @'Fold' a c@ and a value of type @a@.
--
-- A 'Getter' is a legal 'Fold' that just ignores the supplied 'Monoid'
--
-- Unlike a 'Control.Lens.Traversal.Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no lens laws that apply.
type Fold a c = forall f. (Gettable f, Applicative f) => (c -> f c) -> a -> f a


-- | Obtain a 'Fold' by lifting an operation that returns a foldable result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
folding :: (Foldable f, Applicative g, Gettable g) => (a -> f c) -> LensLike g a b c d
folding afc cgd = coerce . traverse_ cgd . afc
{-# INLINE folding #-}

-- | Obtain a 'Fold' from any 'Foldable'.
folded :: Foldable f => Fold (f c) c
folded f = coerce . getFolding . foldMap (Folding . f)
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
cycled :: (Applicative f, Gettable f) => LensLike f a b c d -> LensLike f a b c d
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

-- | Obtain a 'Fold' by filtering a 'Lens', 'Control.Lens.Iso.Iso', 'Getter', 'Fold' or 'Control.Lens.Traversal.Traversal'.
filtered :: (Gettable f, Applicative f) => (c -> Bool) -> LensLike f a b c d -> LensLike f a b c d
filtered p l f = l $ \c -> if p c then f c
                                  else noEffect
{-# INLINE filtered #-}

-- | This allows you to traverse the elements of a 'Control.Lens.Traversal.Traversal' or 'Fold' in the opposite order.
-- This will demote an 'Control.Lens.IndexedTraversal.IndexedTraversal' or 'Control.Lens.IndexedFold.IndexedFold' to a regular 'Control.Lens.Traversal.Traversal' or 'Fold';
-- to preserve the indices, use 'Control.Lens.IndexedFold.ibackwards' instead.
--
-- Note: 'backwards' should have no impact on a 'Getter' 'Setter', 'Lens' or 'Control.Lens.Iso.Iso'.
--
-- To change the direction of an 'Control.Lens.Iso.Iso', use 'from'.
backwards :: LensLike (Backwards f) a b c d -> LensLike f a b c d
backwards l f = forwards . l (Backwards . f)
{-# INLINE backwards #-}

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Control.Lens.Iso.Iso', 'Getter' or 'Control.Lens.Traversal.Traversal' while a predicate holds.
--
-- @'takeWhile' p ≡ 'toListOf' ('takingWhile' p 'folded')@
--
-- >>> toListOf (takingWhile (<=3) folded) [1..]
-- [1,2,3]
takingWhile :: (Gettable f, Applicative f)
            => (c -> Bool)
            -> Getting (Endo (f a)) a a c c
            -> LensLike f a a c c
takingWhile p l f = foldrOf l (\a r -> if p a then f a *> r else noEffect) noEffect
{-# INLINE takingWhile #-}


-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Control.Lens.Iso.Iso', 'Getter' or 'Control.Lens.Traversal.Traversal' while a predicate holds.
--
-- @'dropWhile' p ≡ 'toListOf' ('droppingWhile' p 'folded')@
--
-- >>> toListOf (droppingWhile (<=3) folded) [1..6]
-- [4,5,6]
droppingWhile :: (Gettable f, Applicative f)
              => (c -> Bool)
              -> Getting (Endo (f a)) a a c c
              -> LensLike f a a c c
droppingWhile p l f = foldrOf l (\a r -> if p a then r else f a *> r) noEffect
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
-- 'foldMapOf' ::             'Getter' a c           -> (c -> r) -> a -> r
-- 'foldMapOf' :: 'Monoid' r => 'Fold' a c             -> (c -> r) -> a -> r
-- 'foldMapOf' ::             'Simple' 'Lens' a c      -> (c -> r) -> a -> r
-- 'foldMapOf' ::             'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> r) -> a -> r
-- 'foldMapOf' :: 'Monoid' r => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> r) -> a -> r
-- @
foldMapOf :: Getting r a b c d -> (c -> r) -> a -> r
foldMapOf l f = runAccessor . l (Accessor . f)
{-# INLINE foldMapOf #-}

-- |
-- @'Data.Foldable.fold' = 'foldOf' 'folded'@
--
-- @'foldOf' ≡ 'view'@
--
-- @
-- 'foldOf' ::             'Getter' a m           -> a -> m
-- 'foldOf' :: 'Monoid' m => 'Fold' a m             -> a -> m
-- 'foldOf' ::             'Simple' 'Lens' a m      -> a -> m
-- 'foldOf' ::             'Simple' 'Control.Lens.Iso.Iso' a m       -> a -> m
-- 'foldOf' :: 'Monoid' m => 'Simple' 'Control.Lens.Traversal.Traversal' a m -> a -> m
-- @
foldOf :: Getting c a b c d -> a -> c
foldOf l = runAccessor . l Accessor
{-# INLINE foldOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Control.Lens.Traversal.Traversal'.
--
-- @'Data.Foldable.foldr' ≡ 'foldrOf' 'folded'@
--
-- @
-- 'foldrOf' :: 'Getter' a c           -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf' :: 'Fold' a c             -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf' :: 'Simple' 'Lens' a c      -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> e -> e) -> e -> a -> e
-- @
foldrOf :: Getting (Endo e) a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z
{-# INLINE foldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Control.Lens.Traversal.Traversal'.
--
-- @'Data.Foldable.foldl' ≡ 'foldlOf' 'folded'@
--
-- @
-- 'foldlOf' :: 'Getter' a c           -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf' :: 'Fold' a c             -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf' :: 'Simple' 'Lens' a c      -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (e -> c -> e) -> e -> a -> e
-- @
foldlOf :: Getting (Dual (Endo e)) a b c d -> (e -> c -> e) -> e -> a -> e
foldlOf l f z t = appEndo (getDual (foldMapOf l (Dual . Endo . flip f) t)) z
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
-- 'toListOf' :: 'Getter' a c           -> a -> [c]
-- 'toListOf' :: 'Fold' a c             -> a -> [c]
-- 'toListOf' :: 'Simple' 'Lens' a c      -> a -> [c]
-- 'toListOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> [c]
-- 'toListOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> [c]
-- @
toListOf :: Getting [c] a b c d -> a -> [c]
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
-- ('^..') :: a -> 'Getter' a c           -> [c]
-- ('^..') :: a -> 'Fold' a c             -> [c]
-- ('^..') :: a -> 'Simple' 'Lens' a c      -> [c]
-- ('^..') :: a -> 'Simple' 'Control.Lens.Iso.Iso' a c       -> [c]
-- ('^..') :: a -> 'Simple' 'Control.Lens.Traversal.Traversal' a c -> [c]
-- @
(^..) :: a -> Getting [c] a b c d -> [c]
a ^.. l = foldMapOf l return a

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
-- 'andOf' :: 'Getter' a 'Bool'           -> a -> 'Bool'
-- 'andOf' :: 'Fold' a 'Bool'             -> a -> 'Bool'
-- 'andOf' :: 'Simple' 'Lens' a 'Bool'      -> a -> 'Bool'
-- 'andOf' :: 'Simple' 'Control.Lens.Iso.Iso' a 'Bool'       -> a -> 'Bool'
-- 'andOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a 'Bool' -> a -> 'Bool'
-- @
andOf :: Getting All a b Bool d -> a -> Bool
andOf l = getAll . foldMapOf l All
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
-- 'orOf' :: 'Getter' a 'Bool'           -> a -> 'Bool'
-- 'orOf' :: 'Fold' a 'Bool'             -> a -> 'Bool'
-- 'orOf' :: 'Simple' 'Lens' a 'Bool'      -> a -> 'Bool'
-- 'orOf' :: 'Simple' 'Control.Lens.Iso.Iso' a 'Bool'       -> a -> 'Bool'
-- 'orOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a 'Bool' -> a -> 'Bool'
-- @
orOf :: Getting Any a b Bool d -> a -> Bool
orOf l = getAny . foldMapOf l Any
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
-- 'anyOf' :: 'Getter' a c               -> (c -> 'Bool') -> a -> 'Bool'
-- 'anyOf' :: 'Fold' a c                 -> (c -> 'Bool') -> a -> 'Bool'
-- 'anyOf' :: 'Simple' 'Lens' a b c d      -> (c -> 'Bool') -> a -> 'Bool'
-- 'anyOf' :: 'Simple' 'Control.Lens.Iso.Iso' a b c d       -> (c -> 'Bool') -> a -> 'Bool'
-- 'anyOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a b c d -> (c -> 'Bool') -> a -> 'Bool'
-- @
anyOf :: Getting Any a b c d -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)
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
-- 'allOf' :: 'Getter' a c           -> (c -> 'Bool') -> a -> 'Bool'
-- 'allOf' :: 'Fold' a c             -> (c -> 'Bool') -> a -> 'Bool'
-- 'allOf' :: 'Simple' 'Lens' a c      -> (c -> 'Bool') -> a -> 'Bool'
-- 'allOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> 'Bool') -> a -> 'Bool'
-- 'allOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> 'Bool') -> a -> 'Bool'
-- @
allOf :: Getting All a b c d -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)
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
-- 'productOf' ::          'Getter' a c           -> a -> c
-- 'productOf' :: 'Num' c => 'Fold' a c             -> a -> c
-- 'productOf' ::          'Simple' 'Lens' a c      -> a -> c
-- 'productOf' ::          'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> c
-- 'productOf' :: 'Num' c => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> c
-- @
productOf :: Getting (Product c) a b c d -> a -> c
productOf l = getProduct . foldMapOf l Product
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
-- 'sumOf' ::          'Getter' a c           -> a -> c
-- 'sumOf' :: 'Num' c => 'Fold' a c             -> a -> c
-- 'sumOf' ::          'Simple' 'Lens' a c      -> a -> c
-- 'sumOf' ::          'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> c
-- 'sumOf' :: 'Num' c => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> c
-- @
sumOf :: Getting (Sum c) a b c d -> a -> c
sumOf l = getSum . foldMapOf l Sum
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
-- 'traverseOf_' '_2' :: 'Functor' f => (c -> f e) -> (c1, c) -> f ()
-- 'traverseOf_' 'Data.Either.Lens.traverseLeft' :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ()
-- @
--
-- The rather specific signature of 'traverseOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'traverseOf_' :: 'Functor' f     => 'Getter' a c           -> (c -> f e) -> a -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Fold' a c             -> (c -> f e) -> a -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Simple' 'Lens' a c      -> (c -> f e) -> a -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> f e) -> a -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> f e) -> a -> f ()
-- @
traverseOf_ :: Functor f => Getting (Traversed f) a b c d -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversed . foldMapOf l (Traversed . void . f)
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
-- 'forOf_' :: 'Functor' f     => 'Getter' a c           -> a -> (c -> f e) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Fold' a c             -> a -> (c -> f e) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Simple' 'Lens' a c      -> a -> (c -> f e) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> (c -> f e) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> (c -> f e) -> f ()
-- @
forOf_ :: Functor f => Getting (Traversed f) a b c d -> a -> (c -> f e) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in observed by a 'Fold' on a structure from left to right, ignoring the results.
--
-- @'sequenceA_' ≡ 'sequenceAOf_' 'folded'@
--
-- @
-- 'sequenceAOf_' :: 'Functor' f     => 'Getter' a (f ())           -> a -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Fold' a (f ())             -> a -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Simple' 'Lens' a (f ())      -> a -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Simple' 'Iso' a (f ())       -> a -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Simple' 'Control.Lens.Traversal.Traversal' a (f ()) -> a -> f ()
-- @
sequenceAOf_ :: Functor f => Getting (Traversed f) a b (f ()) d -> a -> f ()
sequenceAOf_ l = getTraversed . foldMapOf l (Traversed . void)
{-# INLINE sequenceAOf_ #-}

-- | Map each target of a 'Fold' on a structure to a monadic action, evaluate these actions from left to right, and ignore the results.
--
-- @'Data.Foldable.mapM_' ≡ 'mapMOf_' 'folded'@
--
-- @
-- 'mapMOf_' :: 'Monad' m => 'Getter' a c           -> (c -> m e) -> a -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Fold' a c             -> (c -> m e) -> a -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Simple' 'Lens' a c      -> (c -> m e) -> a -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> m e) -> a -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> m e) -> a -> m ()
-- @
mapMOf_ :: Monad m => Getting (Sequenced m) a b c d -> (c -> m e) -> a -> m ()
mapMOf_ l f = getSequenced . foldMapOf l (Sequenced . liftM skip . f)
{-# INLINE mapMOf_ #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

-- | 'forMOf_' is 'mapMOf_' with two of its arguments flipped.
--
-- @'Data.Foldable.forM_' ≡ 'forMOf_' 'folded'@
--
-- @
-- 'forMOf_' :: 'Monad' m => 'Getter' a c           -> a -> (c -> m e) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Fold' a c             -> a -> (c -> m e) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Simple' 'Lens' a c      -> a -> (c -> m e) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> (c -> m e) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> (c -> m e) -> m ()
-- @
forMOf_ :: Monad m => Getting (Sequenced m) a b c d -> a -> (c -> m e) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- | Evaluate each monadic action referenced by a 'Fold' on the structure from left to right, and ignore the results.
--
-- @'Data.Foldable.sequence_' ≡ 'sequenceOf_' 'folded'@
--
-- @
-- 'sequenceOf_' :: 'Monad' m => 'Getter' a (m b)           -> a -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Fold' a (m b)             -> a -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Simple' 'Lens' a (m b)      -> a -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' a (m b)       -> a -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' a (m b) -> a -> m ()
-- @
sequenceOf_ :: Monad m => Getting (Sequenced m) a b (m c) d -> a -> m ()
sequenceOf_ l = getSequenced . foldMapOf l (Sequenced . liftM skip)
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- @'asum' ≡ 'asumOf' 'folded'@
--
-- @
-- 'asumOf' :: 'Alternative' f => 'Getter' a c           -> a -> f c
-- 'asumOf' :: 'Alternative' f => 'Fold' a c             -> a -> f c
-- 'asumOf' :: 'Alternative' f => 'Simple' 'Lens' a c      -> a -> f c
-- 'asumOf' :: 'Alternative' f => 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> f c
-- 'asumOf' :: 'Alternative' f => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> f c
-- @
asumOf :: Alternative f => Getting (Endo (f c)) a b (f c) d -> a -> f c
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- @'msum' ≡ 'msumOf' 'folded'@
--
-- @
-- 'msumOf' :: 'MonadPlus' m => 'Getter' a c           -> a -> m c
-- 'msumOf' :: 'MonadPlus' m => 'Fold' a c             -> a -> m c
-- 'msumOf' :: 'MonadPlus' m => 'Simple' 'Lens' a c      -> a -> m c
-- 'msumOf' :: 'MonadPlus' m => 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> m c
-- 'msumOf' :: 'MonadPlus' m => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> m c
-- @
msumOf :: MonadPlus m => Getting (Endo (m c)) a b (m c) d -> a -> m c
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
-- 'elemOf' :: 'Eq' c => 'Getter' a c           -> c -> a -> 'Bool'
-- 'elemOf' :: 'Eq' c => 'Fold' a c             -> c -> a -> 'Bool'
-- 'elemOf' :: 'Eq' c => 'Simple' 'Lens' a c      -> c -> a -> 'Bool'
-- 'elemOf' :: 'Eq' c => 'Simple' 'Control.Lens.Iso.Iso' a c       -> c -> a -> 'Bool'
-- 'elemOf' :: 'Eq' c => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> c -> a -> 'Bool'
-- @
elemOf :: Eq c => Getting Any a b c d -> c -> a -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- @'notElem' ≡ 'notElemOf' 'folded'@
--
-- @
-- 'notElemOf' :: 'Eq' c => 'Getter' a c           -> c -> a -> 'Bool'
-- 'notElemOf' :: 'Eq' c => 'Fold' a c             -> c -> a -> 'Bool'
-- 'notElemOf' :: 'Eq' c => 'Simple' 'Control.Lens.Iso.Iso' a c       -> c -> a -> 'Bool'
-- 'notElemOf' :: 'Eq' c => 'Simple' 'Lens' a c      -> c -> a -> 'Bool'
-- 'notElemOf' :: 'Eq' c => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> c -> a -> 'Bool'
-- @
notElemOf :: Eq c => Getting All a b c d -> c -> a -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- | Map a function over all the targets of a 'Fold' of a container and concatenate the resulting lists.
--
-- @'concatMap' ≡ 'concatMapOf' 'folded'@
--
-- @
-- 'concatMapOf' :: 'Getter' a c           -> (c -> [e]) -> a -> [e]
-- 'concatMapOf' :: 'Fold' a c             -> (c -> [e]) -> a -> [e]
-- 'concatMapOf' :: 'Simple' 'Lens' a c      -> (c -> [e]) -> a -> [e]
-- 'concatMapOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> [e]) -> a -> [e]
-- 'concatMapOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> [e]) -> a -> [e]
-- @
concatMapOf :: Getting [e] a b c d -> (c -> [e]) -> a -> [e]
concatMapOf l ces = runAccessor . l (Accessor . ces)
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
-- 'concatOf' :: 'Getter' a [e]           -> a -> [e]
-- 'concatOf' :: 'Fold' a [e]             -> a -> [e]
-- 'concatOf' :: 'Simple' 'Control.Lens.Iso.Iso' a [e]       -> a -> [e]
-- 'concatOf' :: 'Simple' 'Lens' a [e]      -> a -> [e]
-- 'concatOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a [e] -> a -> [e]
-- @
concatOf :: Getting [e] a b [e] d -> a -> [e]
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
-- 'lengthOf' :: 'Getter' a c           -> a -> 'Int'
-- 'lengthOf' :: 'Fold' a c             -> a -> 'Int'
-- 'lengthOf' :: 'Simple' 'Lens' a c      -> a -> 'Int'
-- 'lengthOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Int'
-- 'lengthOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Int'
-- @
lengthOf :: Getting (Sum Int) a b c d -> a -> Int
lengthOf l = getSum . foldMapOf l (\_ -> Sum 1)
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Control.Lens.Traversal.Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'. See also ('^?').
--
-- @'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'headOf' 'folded'@
--
-- @
-- 'headOf' :: 'Getter' a c           -> a -> 'Maybe' c
-- 'headOf' :: 'Fold' a c             -> a -> 'Maybe' c
-- 'headOf' :: 'Simple' 'Lens' a c      -> a -> 'Maybe' c
-- 'headOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Maybe' c
-- 'headOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Maybe' c
-- @
headOf :: Getting (First c) a b c d -> a -> Maybe c
headOf l = getFirst . foldMapOf l (First . Just)
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
-- ('^?') :: a -> 'Getter' a c           -> 'Maybe' c
-- ('^?') :: a -> 'Fold' a c             -> 'Maybe' c
-- ('^?') :: a -> 'Simple' 'Lens' a c      -> 'Maybe' c
-- ('^?') :: a -> 'Simple' 'Control.Lens.Iso.Iso' a c       -> 'Maybe' c
-- ('^?') :: a -> 'Simple' 'Control.Lens.Traversal.Traversal' a c -> 'Maybe' c
-- @
(^?) :: a -> Getting (First c) a b c d -> Maybe c
a ^? l = getFirst (foldMapOf l (First . Just) a)
{-# INLINE (^?) #-}

-- | Perform a safe 'last' of a 'Fold' or 'Control.Lens.Traversal.Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- @
-- 'lastOf' :: 'Getter' a c           -> a -> 'Maybe' c
-- 'lastOf' :: 'Fold' a c             -> a -> 'Maybe' c
-- 'lastOf' :: 'Simple' 'Lens' a c      -> a -> 'Maybe' c
-- 'lastOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Maybe' c
-- 'lastOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Maybe' c
-- @
lastOf :: Getting (Last c) a b c d -> a -> Maybe c
lastOf l = getLast . foldMapOf l (Last . Just)
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
-- 'nullOf' :: 'Getter' a c           -> a -> 'Bool'
-- 'nullOf' :: 'Fold' a c             -> a -> 'Bool'
-- 'nullOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Bool'
-- 'nullOf' :: 'Simple' 'Lens' a c      -> a -> 'Bool'
-- 'nullOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Bool'
-- @
nullOf :: Getting All a b c d -> a -> Bool
nullOf l = getAll . foldMapOf l (\_ -> All False)
{-# INLINE nullOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold' or 'Control.Lens.Traversal.Traversal'
--
-- Note: maximumOf on a valid 'Control.Lens.Iso.Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- @'maximum' ≡ 'fromMaybe' ('error' "empty") '.' 'maximumOf' 'folded'@
--
-- @
-- 'maximumOf' ::          'Getter' a c           -> a -> 'Maybe' c
-- 'maximumOf' :: 'Ord' c => 'Fold' a c             -> a -> 'Maybe' c
-- 'maximumOf' ::          'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Maybe' c
-- 'maximumOf' ::          'Simple' 'Lens' a c      -> a -> 'Maybe' c
-- 'maximumOf' :: 'Ord' c => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Maybe' c
-- @
maximumOf :: Getting (Max c) a b c d -> a -> Maybe c
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
-- 'minimumOf' ::          'Getter' a c           -> a -> 'Maybe' c
-- 'minimumOf' :: 'Ord' c => 'Fold' a c             -> a -> 'Maybe' c
-- 'minimumOf' ::          'Simple' 'Control.Lens.Iso.Iso' a c       -> a -> 'Maybe' c
-- 'minimumOf' ::          'Simple' 'Lens' a c      -> a -> 'Maybe' c
-- 'minimumOf' :: 'Ord' c => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> a -> 'Maybe' c
-- @
minimumOf :: Getting (Min c) a b c d -> a -> Maybe c
minimumOf l = getMin . foldMapOf l Min
{-# INLINE minimumOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold', 'Control.Lens.Traversal.Traversal', 'Lens', 'Control.Lens.Iso.Iso',
-- or 'Getter' according to a user supplied ordering.
--
-- @'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' "empty") '.' 'maximumByOf' 'folded' cmp@
--
-- @
-- 'maximumByOf' :: 'Getter' a c           -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'maximumByOf' :: 'Fold' a c             -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'maximumByOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'maximumByOf' :: 'Simple' 'Lens' a c      -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'maximumByOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- @
maximumByOf :: Getting (Endo (Maybe c)) a b c d -> (c -> c -> Ordering) -> a -> Maybe c
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
-- 'minimumByOf' :: 'Getter' a c           -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'minimumByOf' :: 'Fold' a c             -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'minimumByOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'minimumByOf' :: 'Simple' 'Lens' a c      -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- 'minimumByOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> c -> 'Ordering') -> a -> 'Maybe' c
-- @
minimumByOf :: Getting (Endo (Maybe c)) a b c d -> (c -> c -> Ordering) -> a -> Maybe c
minimumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then b else a)
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Lens' (or 'Control.Lens.Getter.Getter', 'Control.Lens.Iso.Iso', 'Control.Lens.Fold.Fold', or 'Control.Lens.Traversal.Traversal'),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- @
-- 'findOf' :: 'Getter' a c           -> (c -> 'Bool') -> a -> 'Maybe' c
-- 'findOf' :: 'Fold' a c             -> (c -> 'Bool') -> a -> 'Maybe' c
-- 'findOf' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> 'Bool') -> a -> 'Maybe' c
-- 'findOf' :: 'Simple' 'Lens' a c      -> (c -> 'Bool') -> a -> 'Maybe' c
-- 'findOf' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> 'Bool') -> a -> 'Maybe' c
-- @
findOf :: Getting (First c) a b c d -> (c -> Bool) -> a -> Maybe c
findOf l p = getFirst . foldMapOf l step where
  step c
    | p c       = First (Just c)
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
-- 'foldr1Of' :: 'Getter' a c           -> (c -> c -> c) -> a -> c
-- 'foldr1Of' :: 'Fold' a c             -> (c -> c -> c) -> a -> c
-- 'foldr1Of' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> c -> c) -> a -> c
-- 'foldr1Of' :: 'Simple' 'Lens' a c      -> (c -> c -> c) -> a -> c
-- 'foldr1Of' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> c -> c) -> a -> c
-- @
foldr1Of :: Getting (Endo (Maybe c)) a b c d -> (c -> c -> c) -> a -> c
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
-- 'foldl1Of' :: 'Getter' a c           -> (c -> c -> c) -> a -> c
-- 'foldl1Of' :: 'Fold' a c             -> (c -> c -> c) -> a -> c
-- 'foldl1Of' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> c -> c) -> a -> c
-- 'foldl1Of' :: 'Simple' 'Lens' a c      -> (c -> c -> c) -> a -> c
-- 'foldl1Of' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> c -> c) -> a -> c
-- @
foldl1Of :: Getting (Dual (Endo (Maybe c))) a b c d -> (c -> c -> c) -> a -> c
foldl1Of l f xs = fromMaybe (error "foldl1Of: empty structure") (foldlOf l mf Nothing xs) where
  mf Nothing y = Just y
  mf (Just x) y = Just (f x y)
{-# INLINE foldl1Of #-}

-- | Strictly fold right over the elements of a structure.
--
-- @'Data.Foldable.foldr'' ≡ 'foldrOf'' 'folded'@
--
-- @
-- 'foldrOf'' :: 'Getter' a c           -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf'' :: 'Fold' a c             -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf'' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf'' :: 'Simple' 'Lens' a c      -> (c -> e -> e) -> e -> a -> e
-- 'foldrOf'' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> e -> e) -> e -> a -> e
-- @
foldrOf' :: Getting (Dual (Endo (e -> e))) a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf' l f z0 xs = foldlOf l f' id xs z0
  where f' k x z = k $! f x z
{-# INLINE foldrOf' #-}

-- | Fold over the elements of a structure, associating to the left, but strictly.
--
-- @'Data.Foldable.foldl'' ≡ 'foldlOf'' 'folded'@
--
-- @
-- 'foldlOf'' :: 'Getter' a c           -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf'' :: 'Fold' a c             -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf'' :: 'Simple' 'Control.Lens.Iso.Iso' a c       -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf'' :: 'Simple' 'Lens' a c      -> (e -> c -> e) -> e -> a -> e
-- 'foldlOf'' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (e -> c -> e) -> e -> a -> e
-- @
foldlOf' :: Getting (Endo (e -> e)) a b c d -> (e -> c -> e) -> e -> a -> e
foldlOf' l f z0 xs = foldrOf l f' id xs z0
  where f' x k z = k $! f z x
{-# INLINE foldlOf' #-}

-- | Monadic fold over the elements of a structure, associating to the right,
-- i.e. from right to left.
--
-- @'Data.Foldable.foldrM' ≡ 'foldrMOf' 'folded'@
--
-- @
-- 'foldrMOf' :: 'Monad' m => 'Getter' a c           -> (c -> e -> m e) -> e -> a -> m e
-- 'foldrMOf' :: 'Monad' m => 'Fold' a c             -> (c -> e -> m e) -> e -> a -> m e
-- 'foldrMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> e -> m e) -> e -> a -> m e
-- 'foldrMOf' :: 'Monad' m => 'Simple' 'Lens' a c      -> (c -> e -> m e) -> e -> a -> m e
-- 'foldrMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> e -> m e) -> e -> a -> m e
-- @
foldrMOf :: Monad m
         => Getting (Dual (Endo (e -> m e))) a b c d
         -> (c -> e -> m e) -> e -> a -> m e
foldrMOf l f z0 xs = foldlOf l f' return xs z0
  where f' k x z = f x z >>= k
{-# INLINE foldrMOf #-}

-- | Monadic fold over the elements of a structure, associating to the left,
-- i.e. from left to right.
--
-- @'Data.Foldable.foldlM' ≡ 'foldlMOf' 'folded'@
--
-- @
-- 'foldlMOf' :: 'Monad' m => 'Getter' a c           -> (e -> c -> m e) -> e -> a -> m e
-- 'foldlMOf' :: 'Monad' m => 'Fold' a c             -> (e -> c -> m e) -> e -> a -> m e
-- 'foldlMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Iso.Iso' a c       -> (e -> c -> m e) -> e -> a -> m e
-- 'foldlMOf' :: 'Monad' m => 'Simple' 'Lens' a c      -> (e -> c -> m e) -> e -> a -> m e
-- 'foldlMOf' :: 'Monad' m => 'Simple' 'Control.Lens.Traversal.Traversal' a c -> (e -> c -> m e) -> e -> a -> m e
-- @
foldlMOf :: Monad m
         => Getting (Endo (e -> m e)) a b c d
         -> (e -> c -> m e) -> e -> a -> m e
foldlMOf l f z0 xs = foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}

-- | Useful for storing folds in containers.
newtype ReifiedFold a c = ReifyFold { reflectFold :: Fold a c }
