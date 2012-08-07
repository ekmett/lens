{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Fold
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Fold' a c@ is a generalization of something 'Foldable'. It allows you to
-- extract multiple results from a container. A 'Foldable' container can be
-- characterized by the behavior of @foldMap :: ('Foldable' t, 'Monoid' m) => (c -> m) -> t c -> m@.
-- Since we want to be able to work with monomorphic containers, we generalize this signature to
-- @forall m. 'Monoid' m => (c -> m) -> a -> m@, and then decorate it with 'Const' to obtain
--
-- @type 'Fold' a c = forall m b d. 'Monoid' m => 'Getting' m a b c d@
--
-- In practice the type we use is slightly more complicated to allow for better error messages and
-- for it to be transformed by certain 'Applicative' transformers.
--
-- Every 'Getter' is a valid 'Fold' that simply doesn't use the 'Monoid' it is passed.
--
-- Everything you can do with a 'Foldable' container, you can with with a 'Fold' and there are
-- combinators that generalize the usual 'Foldable' operations here.
----------------------------------------------------------------------------
module Control.Lens.Fold
  (
  -- * Folds
    Fold
  , Furled(..)
  -- ** Building Folds
  , folds
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
-- Unlike a 'Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no lens laws that apply.
type Fold a c = forall r f b d. (Applicative f, Monoid r, Furled r f) => (c -> f d) -> a -> f b

-- | Something we can fold.
class Gettable f => Furled r f | f -> r where
  furled :: r -> f a
  unfurled :: f a -> r

instance Furled r (Accessor r) where
  furled = Accessor
  unfurled = runAccessor

instance Furled r f => Furled (Dual r) (Backwards f) where
  furled = Backwards . furled . getDual
  unfurled = Dual . unfurled . forwards

-- | Build a 'Getter' or 'Fold' from a 'foldMap'-like function.
--
-- > folds :: ((c -> r) -> a -> r) -> (c -> Accessor m d) -> a -> Const m b
folds :: Furled r f => ((c -> r) -> a -> r) -> LensLike f a b c d
folds l f = furled . l (unfurled . f)
{-# INLINE folds #-}

-- | Obtain a 'Fold' by lifting an operation that returns a foldable result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
folding :: (Foldable f, Applicative g, Gettable g) => (a -> f c) -> LensLike g a b c d
folding afc cgd = coerce . traverse_ cgd . afc
{-# INLINE folding #-}

-- | Obtain a 'Fold' from any 'Foldable'.
--
-- > folded = folds foldMap
folded :: Foldable f => Fold (f c) c
folded = folds foldMap
{-# INLINE folded #-}

-- | Fold by repeating the input forever.
--
-- > repeat = toListOf repeated
repeated :: Fold a a
repeated f a = furled as where as = unfurled (f a) `mappend` as

-- | A fold that replicates its input @n@ times.
--
-- > replicate n = toListOf (replicated n)
replicated :: Int -> Fold a a
replicated n0 f a = furled (go n0) where
  m = unfurled (f a)
  go 0 = mempty
  go n = m `mappend` go (n - 1)
{-# INLINE replicated #-}

-- | Transform a fold into a fold that loops over its elements over and over.
--
-- > ghci> toListOf (cycled traverse) [1,2,3]
-- > [1,2,3,1,2,3,..]
cycled :: (Furled r f, Monoid r) => LensLike f a b c d -> LensLike f a b c d
cycled l f a = furled as where as = unfurled (l f a) `mappend` as

-- | Build a fold that unfolds its values from a seed.
--
-- > ghci> unfoldr = toListOf . unfolded
unfolded :: (b -> Maybe (a, b)) -> Fold b a
unfolded f g b0 = go b0 where
  go b = case f b of
    Just (a, b') -> g a *> go b'
    Nothing      -> furled mempty
{-# INLINE unfolded #-}

-- | @x ^. 'iterated' f@ Return an infinite fold of repeated applications of @f@ to @x@.
--
-- > toListOf (iterated f) a = iterate f a
iterated :: (a -> a) -> Fold a a
iterated f g a0 = go a0 where
  go a = g a *> go (f a)
{-# INLINE iterated #-}

-- | Obtain a 'Fold' by filtering a 'Lens', 'Iso', 'Getter', 'Fold' or 'Traversal'.
filtered :: (Furled r f, Monoid r) => (c -> Bool) -> LensLike f a b c d -> LensLike f a b c d
filtered p l f = l $ \c -> furled (if p c then unfurled (f c) else mempty)
{-# INLINE filtered #-}

-- | This allows you to traverse the elements of a 'Traversal' or 'Fold' in the opposite order.
--
-- Note: 'backwards' should have no impact on a 'Getter' 'Setter', 'Lens' or 'Iso'.
--
-- To change the direction of an 'Iso', use 'from'.
backwards :: LensLike (Backwards f) a b c d -> LensLike f a b c d
backwards l f = forwards . l (Backwards . f)
{-# INLINE backwards #-}

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- > takeWhile p = toListOf (takingWhile p folded)
--
-- > ghci> toList (takingWhile (<=3) folded) [1..]
-- > [1,2,3]
takingWhile :: (Monoid r, Furled r f) => (c -> Bool) -> Getting (Endo r) a b c d -> LensLike f a b c d
takingWhile p l f = furled . foldrOf l (\a r -> if p a then unfurled (f a) `mappend` r else mempty) mempty
{-# INLINE takingWhile #-}

-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- > dropWhile p = toListOf (droppingWhile p folded)
--
-- > ghci> toList (dropWhile (<=3) folded) [1..6]
-- > [4,5,6]
droppingWhile :: (Monoid r, Furled r f) => (c -> Bool) -> Getting (Endo r) a b c d -> LensLike f a b c d
droppingWhile p l f = furled . foldrOf l (\a r -> if p a then mempty else mappend r (unfurled (f a))) mempty
{-# INLINE droppingWhile #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- |
-- > foldMap = foldMapOf folded
--
-- > foldMapOf = views
--
-- > foldMapOf ::             Getter a c        -> (c -> r) -> a -> r
-- > foldMapOf :: Monoid r => Fold a c          -> (c -> r) -> a -> r
-- > foldMapOf ::             Lens a b c d      -> (c -> r) -> a -> r
-- > foldMapOf ::             Iso a b c d       -> (c -> r) -> a -> r
-- > foldMapOf :: Monoid r => Traversal a b c d -> (c -> r) -> a -> r
foldMapOf :: Getting r a b c d -> (c -> r) -> a -> r
foldMapOf l f = runAccessor . l (Accessor . f)
{-# INLINE foldMapOf #-}

-- |
-- > fold = foldOf folded
--
-- > foldOf = view
--
-- > foldOf ::             Getter a m        -> a -> m
-- > foldOf :: Monoid m => Fold a m          -> a -> m
-- > foldOf ::             Lens a b m d      -> a -> m
-- > foldOf ::             Iso a b m d       -> a -> m
-- > foldOf :: Monoid m => Traversal a b m d -> a -> m
foldOf :: Getting c a b c d -> a -> c
foldOf l = runAccessor . l Accessor
{-# INLINE foldOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- > foldr = foldrOf folded
--
-- > foldrOf :: Getter a c        -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Fold a c          -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Lens a b c d      -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Iso a b c d       -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Traversal a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf :: Getting (Endo e) a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z
{-# INLINE foldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- > foldl = foldlOf folded
--
-- > foldlOf :: Getter a c        -> (e -> c -> e) -> e -> a -> e
-- > foldlOf :: Fold a c          -> (e -> c -> e) -> e -> a -> e
-- > foldlOf :: Lens a b c d      -> (e -> c -> e) -> e -> a -> e
-- > foldlOf :: Iso a b c d       -> (e -> c -> e) -> e -> a -> e
-- > foldlOf :: Traversal a b c d -> (e -> c -> e) -> e -> a -> e
foldlOf :: Getting (Dual (Endo e)) a b c d -> (e -> c -> e) -> e -> a -> e
foldlOf l f z t = appEndo (getDual (foldMapOf l (Dual . Endo . flip f) t)) z
{-# INLINE foldlOf #-}

-- |
-- > toList = toListOf folded
--
-- > toListOf :: Getter a c        -> a -> [c]
-- > toListOf :: Fold a c          -> a -> [c]
-- > toListOf :: Lens a b c d      -> a -> [c]
-- > toListOf :: Iso a b c d       -> a -> [c]
-- > toListOf :: Traversal a b c d -> a -> [c]
toListOf :: Getting [c] a b c d -> a -> [c]
toListOf l = foldMapOf l return
{-# INLINE toListOf #-}

-- |
-- > and = andOf folded
--
-- > andOf :: Getter a Bool       -> a -> Bool
-- > andOf :: Fold a Bool         -> a -> Bool
-- > andOf :: Lens a b Bool d     -> a -> Bool
-- > andOf :: Iso a b Bool d      -> a -> Bool
-- > andOf :: Traversl a b Bool d -> a -> Bool
andOf :: Getting All a b Bool d -> a -> Bool
andOf l = getAll . foldMapOf l All
{-# INLINE andOf #-}

-- |
-- > or = orOf folded
--
-- > orOf :: Getter a Bool        -> a -> Bool
-- > orOf :: Fold a Bool          -> a -> Bool
-- > orOf :: Lens a b Bool d      -> a -> Bool
-- > orOf :: Iso a b Bool d       -> a -> Bool
-- > orOf :: Traversal a b Bool d -> a -> Bool
orOf :: Getting Any a b Bool d -> a -> Bool
orOf l = getAny . foldMapOf l Any
{-# INLINE orOf #-}

-- |
-- > any = anyOf folded
--
-- > anyOf :: Getter a c        -> (c -> Bool) -> a -> Bool
-- > anyOf :: Fold a c          -> (c -> Bool) -> a -> Bool
-- > anyOf :: Lens a b c d      -> (c -> Bool) -> a -> Bool
-- > anyOf :: Iso a b c d       -> (c -> Bool) -> a -> Bool
-- > anyOf :: Traversal a b c d -> (c -> Bool) -> a -> Bool
anyOf :: Getting Any a b c d -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)
{-# INLINE anyOf #-}

-- |
-- > all = allOf folded
--
-- > allOf :: Getter a c        -> (c -> Bool) -> a -> Bool
-- > allOf :: Fold a c          -> (c -> Bool) -> a -> Bool
-- > allOf :: Lens a b c d      -> (c -> Bool) -> a -> Bool
-- > allOf :: Iso a b c d       -> (c -> Bool) -> a -> Bool
-- > allOf :: Traversal a b c d -> (c -> Bool) -> a -> Bool
allOf :: Getting All a b c d -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)
{-# INLINE allOf #-}

-- |
-- > product = productOf folded
--
-- > productOf ::          Getter a c        -> a -> c
-- > productOf :: Num c => Fold a c          -> a -> c
-- > productOf ::          Lens a b c d      -> a -> c
-- > productOf ::          Iso a b c d       -> a -> c
-- > productOf :: Num c => Traversal a b c d -> a -> c
productOf :: Getting (Product c) a b c d -> a -> c
productOf l = getProduct . foldMapOf l Product
{-# INLINE productOf #-}

-- |
-- > sum = sumOf folded
--
-- > sumOf _1 :: (a, b) -> a
-- > sumOf (folded._1) :: (Foldable f, Num a) => f (a, b) -> a
--
-- > sumOf ::          Getter a c        -> a -> c
-- > sumOf :: Num c => Fold a c          -> a -> c
-- > sumOf ::          Lens a b c d      -> a -> c
-- > sumOf ::          Iso a b c d       -> a -> c
-- > sumOf :: Num c => Traversal a b c d -> a -> c
sumOf :: Getting (Sum c) a b c d -> a -> c
sumOf l = getSum . foldMapOf l Sum
{-# INLINE sumOf #-}

-- |
--
-- When passed a 'Getter', 'traverseOf_' can work over a 'Functor'.
--
-- When passed a 'Fold', 'traverseOf_' requires an 'Applicative'.
--
-- > traverse_ = traverseOf_ folded
--
-- > traverseOf_ _2 :: Functor f => (c -> f e) -> (c1, c) -> f ()
-- > traverseOf_ traverseLeft :: Applicative f => (a -> f b) -> Either a c -> f ()
--
-- The rather specific signature of traverseOf_ allows it to be used as if the signature was either:
--
-- > traverseOf_ :: Functor f     => Getter a c        -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Applicative f => Fold a c          -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Functor f     => Lens a b c d      -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Functor f     => Iso a b c d       -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Applicative f => Traversal a b c d -> (c -> f e) -> a -> f ()
traverseOf_ :: Functor f => Getting (Traversed f) a b c d -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversed . foldMapOf l (Traversed . void . f)
{-# INLINE traverseOf_ #-}

-- |
-- > for_ = forOf_ folded
--
-- > forOf_ :: Functor f     => Getter a c        -> a -> (c -> f e) -> f ()
-- > forOf_ :: Applicative f => Fold a c          -> a -> (c -> f e) -> f ()
-- > forOf_ :: Functor f     => Lens a b c d      -> a -> (c -> f e) -> f ()
-- > forOf_ :: Functor f     => Iso a b c d       -> a -> (c -> f e) -> f ()
-- > forOf_ :: Applicative f => Traversal a b c d -> a -> (c -> f e) -> f ()
forOf_ :: Functor f => Getting (Traversed f) a b c d -> a -> (c -> f e) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- |
-- > sequenceA_ = sequenceAOf_ folded
--
-- > sequenceAOf_ :: Functor f     => Getter a (f ())        -> a -> f ()
-- > sequenceAOf_ :: Applicative f => Fold a (f ())          -> a -> f ()
-- > sequenceAOf_ :: Functor f     => Lens a b (f ()) d      -> a -> f ()
-- > sequenceAOf_ :: Functor f     => Iso a b (f ()) d       -> a -> f ()
-- > sequenceAOf_ :: Applicative f => Traversal a b (f ()) d -> a -> f ()
sequenceAOf_ :: Functor f => Getting (Traversed f) a b (f ()) d -> a -> f ()
sequenceAOf_ l = getTraversed . foldMapOf l (Traversed . void)
{-# INLINE sequenceAOf_ #-}

-- |
-- > mapM_ = mapMOf_ folded
--
-- > mapMOf_ :: Monad m => Getter a c        -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Fold a c          -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Lens a b c d      -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Iso a b c d       -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Traversal a b c d -> (c -> m e) -> a -> m ()
mapMOf_ :: Monad m => Getting (Sequenced m) a b c d -> (c -> m e) -> a -> m ()
mapMOf_ l f = getSequenced . foldMapOf l (Sequenced . liftM skip . f)
{-# INLINE mapMOf_ #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

-- |
-- > forM_ = forMOf_ folded
--
-- > forMOf_ :: Monad m => Getter a c        -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Fold a c          -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Lens a b c d      -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Iso a b c d       -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Traversal a b c d -> a -> (c -> m e) -> m ()
forMOf_ :: Monad m => Getting (Sequenced m) a b c d -> a -> (c -> m e) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- |
-- > sequence_ = sequenceOf_ folded
--
-- > sequenceOf_ :: Monad m => Getter a (m b)        -> a -> m ()
-- > sequenceOf_ :: Monad m => Fold a (m b)          -> a -> m ()
-- > sequenceOf_ :: Monad m => Lens a b (m b) d      -> a -> m ()
-- > sequenceOf_ :: Monad m => Iso a b (m b) d       -> a -> m ()
-- > sequenceOf_ :: Monad m => Traversal a b (m b) d -> a -> m ()
sequenceOf_ :: Monad m => Getting (Sequenced m) a b (m c) d -> a -> m ()
sequenceOf_ l = getSequenced . foldMapOf l (Sequenced . liftM skip)
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > asum = asumOf folded
--
-- > asumOf :: Alternative f => Getter a c        -> a -> f c
-- > asumOf :: Alternative f => Fold a c          -> a -> f c
-- > asumOf :: Alternative f => Lens a b c d      -> a -> f c
-- > asumOf :: Alternative f => Iso a b c d       -> a -> f c
-- > asumOf :: Alternative f => Traversal a b c d -> a -> f c
asumOf :: Alternative f => Getting (Endo (f c)) a b (f c) d -> a -> f c
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > msum = msumOf folded
--
-- > msumOf :: MonadPlus m => Getter a c        -> a -> m c
-- > msumOf :: MonadPlus m => Fold a c          -> a -> m c
-- > msumOf :: MonadPlus m => Lens a b c d      -> a -> m c
-- > msumOf :: MonadPlus m => Iso a b c d       -> a -> m c
-- > msumOf :: MonadPlus m => Traversal a b c d -> a -> m c
msumOf :: MonadPlus m => Getting (Endo (m c)) a b (m c) d -> a -> m c
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- |
-- > elem = elemOf folded
--
-- > elemOf :: Eq c => Getter a c        -> c -> a -> Bool
-- > elemOf :: Eq c => Fold a c          -> c -> a -> Bool
-- > elemOf :: Eq c => Lens a b c d      -> c -> a -> Bool
-- > elemOf :: Eq c => Iso a b c d       -> c -> a -> Bool
-- > elemOf :: Eq c => Traversal a b c d -> c -> a -> Bool
elemOf :: Eq c => Getting Any a b c d -> c -> a -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- |
-- > notElem = notElemOf folded
--
-- > notElemOf :: Eq c => Getter a c        -> c -> a -> Bool
-- > notElemOf :: Eq c => Fold a c          -> c -> a -> Bool
-- > notElemOf :: Eq c => Iso a b c d       -> c -> a -> Bool
-- > notElemOf :: Eq c => Lens a b c d      -> c -> a -> Bool
-- > notElemOf :: Eq c => Traversal a b c d -> c -> a -> Bool
notElemOf :: Eq c => Getting All a b c d -> c -> a -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- |
-- > concatMap = concatMapOf folded
--
-- > concatMapOf :: Getter a c        -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Fold a c          -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Lens a b c d      -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Iso a b c d       -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Traversal a b c d -> (c -> [e]) -> a -> [e]
concatMapOf :: Getting [e] a b c d -> (c -> [e]) -> a -> [e]
concatMapOf l ces = runAccessor . l (Accessor . ces)
{-# INLINE concatMapOf #-}

-- |
-- > concat = concatOf folded
--
-- > concatOf :: Getter a [e]        -> a -> [e]
-- > concatOf :: Fold a [e]          -> a -> [e]
-- > concatOf :: Iso a b [e] d       -> a -> [e]
-- > concatOf :: Lens a b [e] d      -> a -> [e]
-- > concatOf :: Traversal a b [e] d -> a -> [e]
concatOf :: Getting [e] a b [e] d -> a -> [e]
concatOf = view
{-# INLINE concatOf #-}

-- |
-- Note: this can be rather inefficient for large containers.
--
-- > length = lengthOf folded
--
-- > lengthOf _1 :: (a, b) -> Int
-- > lengthOf _1 = 1
-- > lengthOf (folded.folded) :: Foldable f => f (g a) -> Int
--
-- > lengthOf :: Getter a c        -> a -> Int
-- > lengthOf :: Fold a c          -> a -> Int
-- > lengthOf :: Lens a b c d      -> a -> Int
-- > lengthOf :: Iso a b c d       -> a -> Int
-- > lengthOf :: Traversal a b c d -> a -> Int
lengthOf :: Getting (Sum Int) a b c d -> a -> Int
lengthOf l = getSum . foldMapOf l (\_ -> Sum 1)
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- > listToMaybe . toList = headOf folded
--
-- > headOf :: Getter a c        -> a -> Maybe c
-- > headOf :: Fold a c          -> a -> Maybe c
-- > headOf :: Lens a b c d      -> a -> Maybe c
-- > headOf :: Iso a b c d       -> a -> Maybe c
-- > headOf :: Traversal a b c d -> a -> Maybe c
headOf :: Getting (First c) a b c d -> a -> Maybe c
headOf l = getFirst . foldMapOf l (First . Just)
{-# INLINE headOf #-}

-- | Perform a safe 'last' of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- > lastOf :: Getter a c        -> a -> Maybe c
-- > lastOf :: Fold a c          -> a -> Maybe c
-- > lastOf :: Lens a b c d      -> a -> Maybe c
-- > lastOf :: Iso a b c d       -> a -> Maybe c
-- > lastOf :: Traversal a b c d -> a -> Maybe c
lastOf :: Getting (Last c) a b c d -> a -> Maybe c
lastOf l = getLast . foldMapOf l (Last . Just)
{-# INLINE lastOf #-}

-- |
-- Returns 'True' if this 'Fold' or 'Traversal' has no targets in the given container.
--
-- Note: nullOf on a valid 'Iso', 'Lens' or 'Getter' should always return 'False'
--
-- > null = nullOf folded
--
-- This may be rather inefficient compared to the 'null' check of many containers.
--
-- > nullOf _1 :: (a, b) -> Int
-- > nullOf _1 = False
-- > nullOf (folded._1.folded) :: Foldable f => f (g a, b) -> Bool
--
-- > nullOf :: Getter a c        -> a -> Bool
-- > nullOf :: Fold a c          -> a -> Bool
-- > nullOf :: Iso a b c d       -> a -> Bool
-- > nullOf :: Lens a b c d      -> a -> Bool
-- > nullOf :: Traversal a b c d -> a -> Bool
nullOf :: Getting All a b c d -> a -> Bool
nullOf l = getAll . foldMapOf l (\_ -> All False)
{-# INLINE nullOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold' or 'Traversal'
--
-- Note: maximumOf on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- > maximum = fromMaybe (error "empty") . maximumOf folded
--
-- > maximumOf ::          Getter a c        -> a -> Maybe c
-- > maximumOf :: Ord c => Fold a c          -> a -> Maybe c
-- > maximumOf ::          Iso a b c d       -> a -> Maybe c
-- > maximumOf ::          Lens a b c d      -> a -> Maybe c
-- > maximumOf :: Ord c => Traversal a b c d -> a -> Maybe c
maximumOf :: Getting (Max c) a b c d -> a -> Maybe c
maximumOf l = getMax . foldMapOf l Max
{-# INLINE maximumOf #-}

-- |
-- Obtain the minimum element (if any) targeted by a 'Fold' or 'Traversal'
--
-- Note: minimumOf on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- > minimum = fromMaybe (error "empty") . minimumOf folded
--
-- > minimumOf ::          Getter a c        -> a -> Maybe c
-- > minimumOf :: Ord c => Fold a c          -> a -> Maybe c
-- > minimumOf ::          Iso a b c d       -> a -> Maybe c
-- > minimumOf ::          Lens a b c d      -> a -> Maybe c
-- > minimumOf :: Ord c => Traversal a b c d -> a -> Maybe c
minimumOf :: Getting (Min c) a b c d -> a -> Maybe c
minimumOf l = getMin . foldMapOf l Min
{-# INLINE minimumOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso',
-- or 'Getter' according to a user supplied ordering.
--
-- > maximumBy cmp = fromMaybe (error "empty") . maximumByOf folded cmp
--
-- > maximumByOf :: Getter a c        -> (c -> c -> Ordering) -> a -> Maybe c
-- > maximumByOf :: Fold a c          -> (c -> c -> Ordering) -> a -> Maybe c
-- > maximumByOf :: Iso a b c d       -> (c -> c -> Ordering) -> a -> Maybe c
-- > maximumByOf :: Lens a b c d      -> (c -> c -> Ordering) -> a -> Maybe c
-- > maximumByOf :: Traversal a b c d -> (c -> c -> Ordering) -> a -> Maybe c
maximumByOf :: Getting (Endo (Maybe c)) a b c d -> (c -> c -> Ordering) -> a -> Maybe c
maximumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then a else b)
{-# INLINE maximumByOf #-}

-- |
-- Obtain the minimum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso'
-- or 'Getter' according to a user supplied ordering.
--
-- > minimumBy cmp = fromMaybe (error "empty") . minimumByOf folded cmp
--
-- > minimumByOf :: Getter a c        -> (c -> c -> Ordering) -> a -> Maybe c
-- > minimumByOf :: Fold a c          -> (c -> c -> Ordering) -> a -> Maybe c
-- > minimumByOf :: Iso a b c d       -> (c -> c -> Ordering) -> a -> Maybe c
-- > minimumByOf :: Lens a b c d      -> (c -> c -> Ordering) -> a -> Maybe c
-- > minimumByOf :: Traversal a b c d -> (c -> c -> Ordering) -> a -> Maybe c
minimumByOf :: Getting (Endo (Maybe c)) a b c d -> (c -> c -> Ordering) -> a -> Maybe c
minimumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then b else a)
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a lens (or , getter, iso, fold, or traversal),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- > findOf :: Getter a c        -> (c -> Bool) -> a -> Maybe c
-- > findOf :: Fold a c          -> (c -> Bool) -> a -> Maybe c
-- > findOf :: Iso a b c d       -> (c -> Bool) -> a -> Maybe c
-- > findOf :: Lens a b c d      -> (c -> Bool) -> a -> Maybe c
-- > findOf :: Traversal a b c d -> (c -> Bool) -> a -> Maybe c
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
-- > foldr1Of l f = Prelude.foldr1 f . toListOf l
--
-- > foldr1 = foldr1Of folded
--
-- > foldr1Of :: Getter a c        -> (c -> c -> c) -> a -> c
-- > foldr1Of :: Fold a c          -> (c -> c -> c) -> a -> c
-- > foldr1Of :: Iso a b c d       -> (c -> c -> c) -> a -> c
-- > foldr1Of :: Lens a b c d      -> (c -> c -> c) -> a -> c
-- > foldr1Of :: Traversal a b c d -> (c -> c -> c) -> a -> c
foldr1Of :: Getting (Endo (Maybe c)) a b c d -> (c -> c -> c) -> a -> c
foldr1Of l f xs = fromMaybe (error "foldr1Of: empty structure")
                            (foldrOf l mf Nothing xs) where
  mf x Nothing = Just x
  mf x (Just y) = Just (f x y)
{-# INLINE foldr1Of #-}

-- | A variant of 'foldlOf' that has no base case and thus may only be applied to lenses and strutures such
-- that the lens views at least one element of the structure.
--
-- > foldl1Of l f = Prelude.foldl1Of l f . toList
--
-- > foldl1 = foldl1Of folded
--
-- > foldl1Of :: Getter a c        -> (c -> c -> c) -> a -> c
-- > foldl1Of :: Fold a c          -> (c -> c -> c) -> a -> c
-- > foldl1Of :: Iso a b c d       -> (c -> c -> c) -> a -> c
-- > foldl1Of :: Lens a b c d      -> (c -> c -> c) -> a -> c
-- > foldl1Of :: Traversal a b c d -> (c -> c -> c) -> a -> c
foldl1Of :: Getting (Dual (Endo (Maybe c))) a b c d -> (c -> c -> c) -> a -> c
foldl1Of l f xs = fromMaybe (error "foldl1Of: empty structure") (foldlOf l mf Nothing xs) where
  mf Nothing y = Just y
  mf (Just x) y = Just (f x y)
{-# INLINE foldl1Of #-}

-- | Strictly fold right over the elements of a structure.
--
-- > foldr' = foldrOf' folded
--
-- > foldrOf' :: Getter a c        -> (c -> e -> e) -> e -> a -> e
-- > foldrOf' :: Fold a c          -> (c -> e -> e) -> e -> a -> e
-- > foldrOf' :: Iso a b c d       -> (c -> e -> e) -> e -> a -> e
-- > foldrOf' :: Lens a b c d      -> (c -> e -> e) -> e -> a -> e
-- > foldrOf' :: Traversal a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf' :: Getting (Dual (Endo (e -> e))) a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf' l f z0 xs = foldlOf l f' id xs z0
  where f' k x z = k $! f x z
{-# INLINE foldrOf' #-}

-- | Fold over the elements of a structure, associating to the left, but strictly.
--
-- > foldl' = foldlOf' folded
--
-- > foldlOf' :: Getter a c          -> (e -> c -> e) -> e -> a -> e
-- > foldlOf' :: Fold a c            -> (e -> c -> e) -> e -> a -> e
-- > foldlOf' :: Iso a b c d         -> (e -> c -> e) -> e -> a -> e
-- > foldlOf' :: Lens a b c d        -> (e -> c -> e) -> e -> a -> e
-- > foldlOf' :: Traversal a b c d   -> (e -> c -> e) -> e -> a -> e
foldlOf' :: Getting (Endo (e -> e)) a b c d -> (e -> c -> e) -> e -> a -> e
foldlOf' l f z0 xs = foldrOf l f' id xs z0
  where f' x k z = k $! f z x
{-# INLINE foldlOf' #-}

-- | Monadic fold over the elements of a structure, associating to the right,
-- i.e. from right to left.
--
-- > foldrM = foldrMOf folded
--
-- > foldrMOf :: Monad m => Getter a c        -> (c -> e -> m e) -> e -> a -> m e
-- > foldrMOf :: Monad m => Fold a c          -> (c -> e -> m e) -> e -> a -> m e
-- > foldrMOf :: Monad m => Iso a b c d       -> (c -> e -> m e) -> e -> a -> m e
-- > foldrMOf :: Monad m => Lens a b c d      -> (c -> e -> m e) -> e -> a -> m e
-- > foldrMOf :: Monad m => Traversal a b c d -> (c -> e -> m e) -> e -> a -> m e
foldrMOf :: Monad m
         => Getting (Dual (Endo (e -> m e))) a b c d
         -> (c -> e -> m e) -> e -> a -> m e
foldrMOf l f z0 xs = foldlOf l f' return xs z0
  where f' k x z = f x z >>= k
{-# INLINE foldrMOf #-}

-- | Monadic fold over the elements of a structure, associating to the left,
-- i.e. from left to right.
--
-- > foldlM = foldlMOf folded
--
-- > foldlMOf :: Monad m => Getter a c        -> (e -> c -> m e) -> e -> a -> m e
-- > foldlMOf :: Monad m => Fold a c          -> (e -> c -> m e) -> e -> a -> m e
-- > foldlMOf :: Monad m => Iso a b c d       -> (e -> c -> m e) -> e -> a -> m e
-- > foldlMOf :: Monad m => Lens a b c d      -> (e -> c -> m e) -> e -> a -> m e
-- > foldlMOf :: Monad m => Traversal a b c d -> (e -> c -> m e) -> e -> a -> m e
foldlMOf :: Monad m
         => Getting (Endo (e -> m e)) a b c d
         -> (e -> c -> m e) -> e -> a -> m e
foldlMOf l f z0 xs = foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}
