{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Fold
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Fold' s a@ is a generalization of something 'Foldable'. It allows
-- you to extract multiple results from a container. A 'Foldable' container
-- can be characterized by the behavior of
-- @'Data.Foldable.foldMap' :: ('Foldable' t, 'Monoid' m) => (a -> m) -> t a -> m@.
-- Since we want to be able to work with monomorphic containers, we could
-- generalize this signature to @forall m. 'Monoid' m => (a -> m) -> s -> m@,
-- and then decorate it with 'Accessor' to obtain
--
-- @type 'Fold' s a = forall m. 'Monoid' m => 'Getting' m s a@
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
  , IndexedFold

  -- * Getting Started
  , (^..)
  , (^?)
  , (^?!)
  , pre, ipre
  , preview, previews, ipreview, ipreviews
  , preuse, preuses, ipreuse, ipreuses

  , has, hasn't

  -- ** Building Folds
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
  , nullOf, notNullOf
  , firstOf, lastOf
  , maximumOf, minimumOf
  , maximumByOf, minimumByOf
  , findOf
  , foldrOf', foldlOf'
  , foldr1Of, foldl1Of
  , foldr1Of', foldl1Of'
  , foldrMOf, foldlMOf

  -- * Indexed Folds
  , (^@..)
  , (^@?)
  , (^@?!)

  -- ** Indexed Folding
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf
  , ianyOf
  , iallOf
  , itraverseOf_
  , iforOf_
  , imapMOf_
  , iforMOf_
  , iconcatMapOf
  , ifindOf
  , ifoldrOf'
  , ifoldlOf'
  , ifoldrMOf
  , ifoldlMOf
  , itoListOf

  -- ** Building Indexed Folds
  , ifiltered
  , itakingWhile
  , idroppingWhile

  -- * Deprecated
  , headOf

  -- * Internal types
  , Leftmost
  , Rightmost
  , Traversed
  , Sequenced
  ) where

import Control.Applicative as Applicative
import Control.Applicative.Backwards
import Control.Comonad
import Control.Lens.Getter
import Control.Lens.Internal.Fold
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Magma
import Control.Lens.Type
import Control.Monad as Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable as Foldable
import Data.Functor.Compose
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Data.Traversable

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Function
-- >>> import Data.List.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> import Control.DeepSeq (NFData (..), force)
-- >>> import Control.Exception (evaluate)
-- >>> import Data.Maybe (fromMaybe)
-- >>> import System.Timeout (timeout)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let timingOut :: NFData a => a -> IO a; timingOut = fmap (fromMaybe (error "timeout")) . timeout (5*10^6) . evaluate . force

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Use curry" #-}

infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!

--------------------------
-- Folds
--------------------------

-- | Obtain a 'Fold' by lifting an operation that returns a 'Foldable' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
--
-- >>> [1,2,3,4]^..folding tail
-- [2,3,4]
folding :: (Foldable f, Contravariant g, Applicative g) => (s -> f a) -> LensLike g s t a b
folding sfa agb = coerce . traverse_ agb . sfa
{-# INLINE folding #-}

-- | Obtain a 'Fold' from any 'Foldable'.
--
-- >>> Just 3^..folded
-- [3]
--
-- >>> Nothing^..folded
-- []
--
-- >>> [(1,2),(3,4)]^..folded.both
-- [1,2,3,4]
folded :: Foldable f => Fold (f a) a
folded f = coerce . getFolding . foldMap (Folding #. f)
{-# INLINE folded #-}

-- | 'Fold' by repeating the input forever.
--
-- @
-- 'repeat' ≡ 'toListOf' 'repeated'
-- @
--
-- >>> timingOut $ 5^..taking 20 repeated
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
repeated :: Fold a a
repeated f a = as where as = f a *> as
{-# INLINE repeated #-}

-- | A 'Fold' that replicates its input @n@ times.
--
-- @
-- 'replicate' n ≡ 'toListOf' ('replicated' n)
-- @
--
-- >>> 5^..replicated 20
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
replicated :: Int -> Fold a a
replicated n0 f a = go n0 where
  m = f a
  go 0 = noEffect
  go n = m *> go (n - 1)
{-# INLINE replicated #-}

-- | Transform a 'Fold' into a 'Fold' that loops over its elements over and over.
--
-- >>> timingOut $ [1,2,3]^..taking 7 (cycled traverse)
-- [1,2,3,1,2,3,1]
cycled :: (Contravariant f, Applicative f) => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a *> as
{-# INLINE cycled #-}

-- | Build a 'Fold' that unfolds its values from a seed.
--
-- @
-- 'Prelude.unfoldr' ≡ 'toListOf' '.' 'unfolded'
-- @
--
-- >>> 10^..unfolded (\b -> if b == 0 then Nothing else Just (b, b-1))
-- [10,9,8,7,6,5,4,3,2,1]
unfolded :: (b -> Maybe (a, b)) -> Fold b a
unfolded f g b0 = go b0 where
  go b = case f b of
    Just (a, b') -> g a *> go b'
    Nothing      -> noEffect
{-# INLINE unfolded #-}

-- | @x '^.' 'iterated' f@ returns an infinite 'Fold' of repeated applications of @f@ to @x@.
--
-- @
-- 'toListOf' ('iterated' f) a ≡ 'iterate' f a
-- @
iterated :: (a -> a) -> Fold a a
iterated f g a0 = go a0 where
  go a = g a *> go (f a)
{-# INLINE iterated #-}

-- | Obtain a 'Fold' that can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- Note: This is /not/ a legal 'Traversal', unless you are very careful not to invalidate the predicate on the target.
--
-- Note: This is also /not/ a legal 'Prism', unless you are very careful not to inject a value that matches the predicate.
--
-- As a counter example, consider that given @evens = 'filtered' 'even'@ the second 'Traversal' law is violated:
--
-- @
-- 'Control.Lens.Setter.over' evens 'succ' '.' 'Control.Lens.Setter.over' evens 'succ' '/=' 'Control.Lens.Setter.over' evens ('succ' '.' 'succ')
-- @
--
-- So, in order for this to qualify as a legal 'Traversal' you can only use it for actions that preserve the result of the predicate!
--
-- >>> [1..10]^..folded.filtered even
-- [2,4,6,8,10]
--
-- This will preserve an index if it is present.
filtered :: (Choice p, Applicative f) => (a -> Bool) -> Overloaded' p f a a
filtered p = dimap (\x -> if p x then Right x else Left x) (either pure id) . right'
{-# INLINE filtered #-}

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @
-- 'takeWhile' p ≡ 'toListOf' ('takingWhile' p 'folded')
-- @
--
-- >>> timingOut $ toListOf (takingWhile (<=3) folded) [1..]
-- [1,2,3]
--
-- @
-- 'takingWhile' :: (a -> 'Bool') -> 'Fold' s a                         -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Getter' s a                       -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                   -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Lens'' s a                        -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Prism'' s a                       -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Iso'' s a                         -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Action' m s a                     -> 'MonadicFold' m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MonadicFold' m s a                -> 'MonadicFold' m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedAction' i m s a            -> 'IndexedMonadicFold' i m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMonadicFold' i m s a       -> 'IndexedMonadicFold' i m s a
-- @
--
-- /Note:/ When applied to a 'Traversal', 'takingWhile' yields something that can be used as if it were a 'Traversal', but
-- which is not a 'Traversal' per the laws, unless you are careful to ensure that you do not invalidate the predicate when
-- writing back through it.
takingWhile :: (Conjoined p, Applicative f) => (a -> Bool) -> Over p (TakingWhile p f a a) s t a a -> Over p f s t a a
takingWhile p l pafb = fmap runMagma . traverse (corep pafb) . runTakingWhile . l flag where
  flag = cotabulate $ \wa -> let a = extract wa; r = p a in TakingWhile r a $ \pr ->
    if pr && r then Magma () wa else MagmaPure a
{-# INLINE takingWhile #-}

-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @
-- 'dropWhile' p ≡ 'toListOf' ('droppingWhile' p 'folded')
-- @
--
-- >>> toListOf (droppingWhile (<=3) folded) [1..6]
-- [4,5,6]
--
-- >>> toListOf (droppingWhile (<=3) folded) [1,6,1]
-- [6,1]
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'Fold' s a                         -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Getter' s a                       -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                   -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Lens'' s a                        -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Prism'' s a                       -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Iso'' s a                         -> 'Fold' s a                -- see notes
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingTraversal'' s a    -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingLens'' s a         -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingGetter' s a        -> 'IndexPreservingFold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingFold' s a          -> 'IndexPreservingFold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingAction' m s a      -> 'IndexPreservingFold' s a
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingMonadicFold' m s a -> 'IndexPreservingMonadicFold' m s a
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedAction' i m s a            -> 'IndexedFold' i s a
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMonadicFold' i m s a       -> 'IndexedMonadicFold' i m s a
-- @
--
-- Note: Many uses of this combinator will yield something that meets the types, but not the laws of a valid
-- 'Traversal' or 'IndexedTraversal'. The 'Traversal' and 'IndexedTraversal' laws are only satisfied if the
-- new values you assign also pass the predicate! Otherwise subsequent traversals will visit fewer elements
-- and 'Traversal' fusion is not sound.
droppingWhile :: (Conjoined p, Profunctor q, Applicative f)
              => (a -> Bool)
              -> Overloading p q (Compose (State Bool) f) s t a a
              -> Overloading p q f s t a a
droppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = cotabulate $ \wa -> Compose $ state $ \b -> let
      a = extract wa
      b' = b && p a
    in (if b' then pure a else corep f wa, b')
{-# INLINE droppingWhile #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- | @
-- 'Data.Foldable.foldMap' = 'foldMapOf' 'folded'
-- @
--
-- @
-- 'foldMapOf' ≡ 'views'
-- 'ifoldMapOf' l = 'foldMapOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldMapOf' ::             'Getter' s a     -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r => 'Fold' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::             'Lens'' s a      -> (a -> r) -> s -> r
-- 'foldMapOf' ::             'Iso'' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r => 'Traversal'' s a -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r => 'Prism'' s a     -> (a -> r) -> s -> r
-- @
--
-- @
-- 'foldMapOf' :: 'Getting' r s a -> (a -> r) -> s -> r
-- @
foldMapOf :: Profunctor p => Accessing p r s a -> p a r -> s -> r
foldMapOf l f = runAccessor #. l (Accessor #. f)
{-# INLINE foldMapOf #-}

-- | @
-- 'Data.Foldable.fold' = 'foldOf' 'folded'
-- @
--
-- @
-- 'foldOf' ≡ 'view'
-- @
--
-- @
-- 'foldOf' ::             'Getter' s m     -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Fold' s m       -> s -> m
-- 'foldOf' ::             'Lens'' s m      -> s -> m
-- 'foldOf' ::             'Iso'' s m       -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Traversal'' s m -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Prism'' s m     -> s -> m
-- @
foldOf :: Getting a s a -> s -> a
foldOf l = runAccessor #. l Accessor
{-# INLINE foldOf #-}

-- | Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @
-- 'Data.Foldable.foldr' ≡ 'foldrOf' 'folded'
-- @
--
-- @
-- 'foldrOf' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Prism'' s a     -> (a -> r -> r) -> r -> s -> r
-- @
--
-- @
-- 'ifoldrOf' l ≡ 'foldrOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldrOf' :: 'Getting' ('Endo' r) s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf :: Profunctor p => Accessing p (Endo r) s a -> p a (r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z `rmap` foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

-- | Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @
-- 'Data.Foldable.foldl' ≡ 'foldlOf' 'folded'
-- @
--
-- @
-- 'foldlOf' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Prism'' s a     -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf l f z = (flip appEndo z .# getDual) `rmap` foldMapOf l (Dual #. Endo #. flip f)
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
-- 'toListOf' :: 'Getter' s a     -> s -> [a]
-- 'toListOf' :: 'Fold' s a       -> s -> [a]
-- 'toListOf' :: 'Lens'' s a      -> s -> [a]
-- 'toListOf' :: 'Iso'' s a       -> s -> [a]
-- 'toListOf' :: 'Traversal'' s a -> s -> [a]
-- 'toListOf' :: 'Prism'' s a     -> s -> [a]
-- @
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

-- | A convenient infix (flipped) version of 'toListOf'.
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
-- ('^..') :: s -> 'Getter' s a     -> [a]
-- ('^..') :: s -> 'Fold' s a       -> [a]
-- ('^..') :: s -> 'Lens'' s a      -> [a]
-- ('^..') :: s -> 'Iso'' s a       -> [a]
-- ('^..') :: s -> 'Traversal'' s a -> [a]
-- ('^..') :: s -> 'Prism'' s a     -> [a]
-- @
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

-- | Returns 'True' if every target of a 'Fold' is 'True'.
--
-- >>> andOf both (True,False)
-- False
-- >>> andOf both (True,True)
-- True
--
-- @
-- 'Data.Foldable.and' ≡ 'andOf' 'folded'
-- @
--
-- @
-- 'andOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'andOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'andOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'andOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
andOf :: Getting All s Bool -> s -> Bool
andOf l = getAll #. foldMapOf l All
{-# INLINE andOf #-}

-- | Returns 'True' if any target of a 'Fold' is 'True'.
--
-- >>> orOf both (True,False)
-- True
-- >>> orOf both (False,False)
-- False
--
-- @
-- 'Data.Foldable.or' ≡ 'orOf' 'folded'
-- @
--
-- @
-- 'orOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'orOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'orOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'orOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
orOf :: Getting Any s Bool -> s -> Bool
orOf l = getAny #. foldMapOf l Any
{-# INLINE orOf #-}

-- | Returns 'True' if any target of a 'Fold' satisfies a predicate.
--
-- >>> anyOf both (=='x') ('x','y')
-- True
-- >>> import Data.Data.Lens
-- >>> anyOf biplate (== "world") (((),2::Int),"hello",("world",11))
-- True
--
-- @
-- 'Data.Foldable.any' ≡ 'anyOf' 'folded'
-- @
--
-- @
-- 'ianyOf' l ≡ 'allOf' l '.' 'Indexed'
-- @
--
-- @
-- 'anyOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
anyOf :: Profunctor p => Accessing p Any s a -> p a Bool -> s -> Bool
anyOf l f = getAny #. foldMapOf l (Any #. f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold' satisfies a predicate.
--
-- >>> allOf both (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @
-- 'Data.Foldable.all' ≡ 'allOf' 'folded'
-- @
--
-- @
-- 'iallOf' l = 'allOf' l '.' 'Indexed'
-- @
--
-- @
-- 'allOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
allOf :: Profunctor p => Accessing p All s a -> p a Bool -> s -> Bool
allOf l f = getAll #. foldMapOf l (All #. f)
{-# INLINE allOf #-}

-- | Calculate the 'Product' of every number targeted by a 'Fold'.
--
-- >>> productOf both (4,5)
-- 20
-- >>> productOf folded [1,2,3,4,5]
-- 120
--
-- @
-- 'Data.Foldable.product' ≡ 'productOf' 'folded'
-- @

-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Sum' '.' 'foldMapOf'@
--
-- @
-- 'productOf' :: 'Num' a => 'Getter' s a     -> s -> a
-- 'productOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Lens'' s a      -> s -> a
-- 'productOf' :: 'Num' a => 'Iso'' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'productOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
productOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
productOf l = foldlOf' l (*) 1
{-# INLINE productOf #-}

-- | Calculate the 'Sum' of every number targeted by a 'Fold'.
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
-- @
-- 'Data.Foldable.sum' ≡ 'sumOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Sum' '.' 'foldMapOf'@
--
-- @
-- 'sumOf' '_1' :: 'Num' a => (a, b) -> a
-- 'sumOf' ('folded' '.' 'Control.Lens.Tuple._1') :: ('Foldable' f, 'Num' a) => f (a, b) -> a
-- @
--
-- @
-- 'sumOf' :: 'Num' a => 'Getter' s a     -> s -> a
-- 'sumOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Lens'' s a      -> s -> a
-- 'sumOf' :: 'Num' a => 'Iso'' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'sumOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
sumOf l = foldlOf' l (+) 0
{-# INLINE sumOf #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor')-based answer,
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
-- @
-- 'Data.Foldable.traverse_' ≡ 'traverseOf_' 'folded'
-- @
--
-- @
-- 'traverseOf_' '_2' :: 'Functor' f => (c -> f r) -> (d, c) -> f ()
-- 'traverseOf_' 'Control.Lens.Prism._Left' :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ()
-- @
--
-- @
-- 'itraverseOf_' l ≡ 'traverseOf_' l '.' 'Indexed'
-- @
--
-- The rather specific signature of 'traverseOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'traverseOf_' :: 'Functor' f     => 'Getter' s a     -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Fold' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Lens'' s a      -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Iso'' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Traversal'' s a -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Prism'' s a     -> (a -> f r) -> s -> f ()
-- @
traverseOf_ :: (Profunctor p, Functor f) => Accessing p (Traversed r f) s a -> p a (f r) -> s -> f ()
traverseOf_ l f = void . getTraversed #. foldMapOf l (Traversed #. f)
{-# INLINE traverseOf_ #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor')-based answer,
-- but unlike 'Control.Lens.Traversal.forOf' do not construct a new structure. 'forOf_' generalizes
-- 'Data.Foldable.for_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'forOf_' can work over any 'Functor', but when passed a 'Fold', 'forOf_' requires
-- an 'Applicative'.
--
-- @
-- 'for_' ≡ 'forOf_' 'folded'
-- @
--
-- >>> forOf_ both ("hello","world") putStrLn
-- hello
-- world
--
-- The rather specific signature of 'forOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'iforOf_' l s ≡ 'forOf_' l s '.' 'Indexed'
-- @
--
-- @
-- 'forOf_' :: 'Functor' f     => 'Getter' s a     -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Fold' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Lens'' s a      -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Iso'' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Traversal'' s a -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Prism'' s a     -> s -> (a -> f r) -> f ()
-- @
forOf_ :: (Profunctor p, Functor f) => Accessing p (Traversed r f) s a -> s -> p a (f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in observed by a 'Fold' on a structure from left to right, ignoring the results.
--
-- @
-- 'sequenceA_' ≡ 'sequenceAOf_' 'folded'
-- @
--
-- >>> sequenceAOf_ both (putStrLn "hello",putStrLn "world")
-- hello
-- world
--
-- @
-- 'sequenceAOf_' :: 'Functor' f     => 'Getter' s (f a)     -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Fold' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Lens'' s (f a)      -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Iso'' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Traversal'' s (f a) -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Prism'' s (f a)     -> s -> f ()
-- @
sequenceAOf_ :: Functor f => Getting (Traversed a f) s (f a) -> s -> f ()
sequenceAOf_ l = void . getTraversed #. foldMapOf l Traversed
{-# INLINE sequenceAOf_ #-}

-- | Map each target of a 'Fold' on a structure to a monadic action, evaluate these actions from left to right, and ignore the results.
--
-- >>> mapMOf_ both putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.mapM_' ≡ 'mapMOf_' 'folded'
-- @
--
-- @
-- 'mapMOf_' :: 'Monad' m => 'Getter' s a     -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Fold' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Lens'' s a      -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Iso'' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Traversal'' s a -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Prism'' s a     -> (a -> m r) -> s -> m ()
-- @
mapMOf_ :: (Profunctor p, Monad m) => Accessing p (Sequenced r m) s a -> p a (m r) -> s -> m ()
mapMOf_ l f = liftM skip . getSequenced #. foldMapOf l (Sequenced #. f)
{-# INLINE mapMOf_ #-}

-- | 'forMOf_' is 'mapMOf_' with two of its arguments flipped.
--
-- >>> forMOf_ both ("hello","world") putStrLn
-- hello
-- world
--
-- @
-- 'Data.Foldable.forM_' ≡ 'forMOf_' 'folded'
-- @
--
-- @
-- 'forMOf_' :: 'Monad' m => 'Getter' s a     -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Fold' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Lens'' s a      -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Iso'' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Traversal'' s a -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Prism'' s a     -> s -> (a -> m r) -> m ()
-- @
forMOf_ :: (Profunctor p, Monad m) => Accessing p (Sequenced r m) s a -> s -> p a (m r) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- | Evaluate each monadic action referenced by a 'Fold' on the structure from left to right, and ignore the results.
--
-- >>> sequenceOf_ both (putStrLn "hello",putStrLn "world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.sequence_' ≡ 'sequenceOf_' 'folded'
-- @
--
-- @
-- 'sequenceOf_' :: 'Monad' m => 'Getter' s (m a)     -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Fold' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Lens'' s (m a)      -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Iso'' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Traversal'' s (m a) -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Prism'' s (m a)     -> s -> m ()
-- @
sequenceOf_ :: Monad m => Getting (Sequenced a m) s (m a) -> s -> m ()
sequenceOf_ l = liftM skip . getSequenced #. foldMapOf l Sequenced
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- >>> asumOf both ("hello","world")
-- "helloworld"
--
-- >>> asumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'asum' ≡ 'asumOf' 'folded'
-- @
--
-- @
-- 'asumOf' :: 'Alternative' f => 'Getter' s (f a)     -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Fold' s (f a)       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Lens'' s (f a)      -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Iso'' s (f a)       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Traversal'' s (f a) -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Prism'' s (f a)     -> s -> f a
-- @
asumOf :: Alternative f => Getting (Endo (f a)) s (f a) -> s -> f a
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- >>> msumOf both ("hello","world")
-- "helloworld"
--
-- >>> msumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'msum' ≡ 'msumOf' 'folded'
-- @
--
-- @
-- 'msumOf' :: 'MonadPlus' m => 'Getter' s (m a)     -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Fold' s (m a)       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Lens'' s (m a)      -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Iso'' s (m a)       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Traversal'' s (m a) -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Prism'' s (m a)     -> s -> m a
-- @
msumOf :: MonadPlus m => Getting (Endo (m a)) s (m a) -> s -> m a
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- | Does the element occur anywhere within a given 'Fold' of the structure?
--
-- >>> elemOf both "hello" ("hello","world")
-- True
--
-- @
-- 'elem' ≡ 'elemOf' 'folded'
-- @
--
-- @
-- 'elemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- >>> notElemOf each 'd' ('a','b','c')
-- True
--
-- >>> notElemOf each 'a' ('a','b','c')
-- False
--
-- @
-- 'notElem' ≡ 'notElemOf' 'folded'
-- @
--
-- @
-- 'notElemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
notElemOf :: Eq a => Getting All s a -> a -> s -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- | Map a function over all the targets of a 'Fold' of a container and concatenate the resulting lists.
--
-- >>> concatMapOf both (\x -> [x, x + 1]) (1,3)
-- [1,2,3,4]
--
-- @
-- 'concatMap' ≡ 'concatMapOf' 'folded'
-- @
--
-- @
-- 'concatMapOf' :: 'Getter' s a     -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Fold' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Lens'' s a      -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Iso'' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Traversal'' s a -> (a -> [r]) -> s -> [r]
-- @
concatMapOf :: Profunctor p => Accessing p [r] s a -> p a [r] -> s -> [r]
concatMapOf l ces = runAccessor #. l (Accessor #. ces)
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
-- 'concatOf' :: 'Getter' s [r]     -> s -> [r]
-- 'concatOf' :: 'Fold' s [r]       -> s -> [r]
-- 'concatOf' :: 'Iso'' s [r]       -> s -> [r]
-- 'concatOf' :: 'Lens'' s [r]      -> s -> [r]
-- 'concatOf' :: 'Traversal'' s [r] -> s -> [r]
-- @
concatOf :: Getting [r] s [r] -> s -> [r]
concatOf l = runAccessor #. l Accessor
{-# INLINE concatOf #-}


-- | Calculate the number of targets there are for a 'Fold' in a given container.
--
-- /Note:/ This can be rather inefficient for large containers and just like 'length',
-- this will not terminate for infinite folds.
--
-- @
-- 'length' ≡ 'lengthOf' 'folded'
-- @
--
-- >>> lengthOf _1 ("hello",())
-- 1
--
-- >>> lengthOf traverse [1..10]
-- 10
--
-- >>> lengthOf (traverse.traverse) [[1,2],[3,4],[5,6]]
-- 6
--
-- @
-- 'lengthOf' ('folded' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a) -> 'Int'
-- @
--
-- @
-- 'lengthOf' :: 'Getter' s a     -> s -> 'Int'
-- 'lengthOf' :: 'Fold' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Lens'' s a      -> s -> 'Int'
-- 'lengthOf' :: 'Iso'' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Traversal'' s a -> s -> 'Int'
-- @
lengthOf :: Getting (Endo (Endo Int)) s a -> s -> Int
lengthOf l = foldlOf' l (\a _ -> a + 1) 0
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- When using a 'Traversal' as a partial 'Lens', or a 'Fold' as a partial 'Getter' this can be a convenient
-- way to extract the optional value.
--
-- Note: if you get stack overflows due to this, you may want to use 'firstOf' instead, which can deal
-- more gracefully with heavily left-biased trees.
--
-- >>> Left 4 ^?_Left
-- Just 4
--
-- >>> Right 4 ^?_Left
-- Nothing
--
-- >>> "world" ^? ix 3
-- Just 'l'
--
-- >>> "world" ^? ix 20
-- Nothing
--
-- @
-- ('^?') ≡ 'flip' 'preview'
-- @
--
-- @
-- ('^?') :: s -> 'Getter' s a     -> 'Maybe' a
-- ('^?') :: s -> 'Fold' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Lens'' s a      -> 'Maybe' a
-- ('^?') :: s -> 'Iso'' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Traversal'' s a -> 'Maybe' a
-- @
(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)
{-# INLINE (^?) #-}

-- | Perform an *UNSAFE* 'head' of a 'Fold' or 'Traversal' assuming that it is there.
--
-- >>> Left 4 ^?! _Left
-- 4
--
-- >>> "world" ^?! ix 3
-- 'l'
--
-- @
-- ('^?!') :: s -> 'Getter' s a     -> a
-- ('^?!') :: s -> 'Fold' s a       -> a
-- ('^?!') :: s -> 'Lens'' s a      -> a
-- ('^?!') :: s -> 'Iso'' s a       -> a
-- ('^?!') :: s -> 'Traversal'' s a -> a
-- @
(^?!) :: s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

-- | Retrieve the 'First' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'ala' 'First' '.' 'foldMapOf'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but may have worse
-- constant factors.
--
-- >>> firstOf traverse [1..10]
-- Just 1
--
-- >>> firstOf both (1,2)
-- Just 1
--
-- >>> firstOf ignored ()
-- Nothing
--
-- @
-- 'firstOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'firstOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'firstOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
firstOf :: Getting (Leftmost a) s a -> s -> Maybe a
firstOf l = getLeftmost . foldMapOf l LLeaf
{-# INLINE firstOf #-}

-- | Retrieve the 'Last' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'ala' 'Last' '.' 'foldMapOf'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but may have worse
-- constant factors.
--
-- >>> lastOf traverse [1..10]
-- Just 10
--
-- >>> lastOf both (1,2)
-- Just 2
--
-- >>> lastOf ignored ()
-- Nothing
--
-- @
-- 'lastOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'lastOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'lastOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
lastOf :: Getting (Rightmost a) s a -> s -> Maybe a
lastOf l = getRightmost . foldMapOf l RLeaf
{-# INLINE lastOf #-}

-- | Returns 'True' if this 'Fold' or 'Traversal' has no targets in the given container.
--
-- Note: 'nullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'False'.
--
-- @
-- 'null' ≡ 'nullOf' 'folded'
-- @
--
-- This may be rather inefficient compared to the 'null' check of many containers.
--
-- >>> nullOf _1 (1,2)
-- False
--
-- >>> nullOf ignored ()
-- True
--
-- >>> nullOf traverse []
-- True
--
-- >>> nullOf (element 20) [1..10]
-- True
--
-- @
-- 'nullOf' ('folded' '.' '_1' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a, b) -> 'Bool'
-- @
--
-- @
-- 'nullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'nullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'nullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
nullOf :: Getting All s a -> s -> Bool
nullOf = hasn't
{-# INLINE nullOf #-}

-- | Returns 'True' if this 'Fold' or 'Traversal' has any targets in the given container.
--
-- A more \"conversational\" alias for this combinator is 'has'.
--
-- Note: 'notNullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'True'.
--
-- @
-- 'null' ≡ 'notNullOf' 'folded'
-- @
--
-- This may be rather inefficient compared to the @'not' '.' 'null'@ check of many containers.
--
-- >>> notNullOf _1 (1,2)
-- True
--
-- >>> notNullOf traverse [1..10]
-- True
--
-- >>> notNullOf folded []
-- False
--
-- >>> notNullOf (element 20) [1..10]
-- False
--
-- @
-- 'notNullOf' ('folded' '.' '_1' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a, b) -> 'Bool'
-- @
--
-- @
-- 'notNullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'notNullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'notNullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
notNullOf :: Getting Any s a -> s -> Bool
notNullOf = has
{-# INLINE notNullOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold' or 'Traversal' safely.
--
-- Note: 'maximumOf' on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- >>> maximumOf traverse [1..10]
-- Just 10
--
-- >>> maximumOf traverse []
-- Nothing
--
-- >>> maximumOf (folded.filtered even) [1,4,3,6,7,9,2]
-- Just 6
--
-- @
-- 'maximum' ≡ 'fromMaybe' ('error' \"empty\") '.' 'maximumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
-- @'rmap' 'getMax' ('foldMapOf' l 'Max')@ has lazier semantics but could leak memory.
--
-- @
-- 'maximumOf' :: 'Ord' a => 'Getter' s a     -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Iso'' s a       -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Lens'' s a      -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
maximumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
maximumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! max x y
{-# INLINE maximumOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold' or 'Traversal' safely.
--
-- Note: 'minimumOf' on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- >>> minimumOf traverse [1..10]
-- Just 1
--
-- >>> minimumOf traverse []
-- Nothing
--
-- >>> minimumOf (folded.filtered even) [1,4,3,6,7,9,2]
-- Just 2
--
-- @
-- 'minimum' ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
-- @'rmap' 'getMin' ('foldMapOf' l 'Min')@ has lazier semantics but could leak memory.
--
--
-- @
-- 'minimumOf' :: 'Ord' a => 'Getter' s a     -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Iso'' s a       -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Lens'' s a      -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
minimumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
minimumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! min x y
{-# INLINE minimumOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso',
-- or 'Getter' according to a user supplied 'Ordering'.
--
-- >>> maximumByOf traverse (compare `on` length) ["mustard","relish","ham"]
-- Just "mustard"
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
--
-- @
-- 'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumByOf' 'folded' cmp
-- @
--
-- @
-- 'maximumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
maximumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then x else y
{-# INLINE maximumByOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso'
-- or 'Getter' according to a user supplied 'Ordering'.
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
--
-- >>> minimumByOf traverse (compare `on` length) ["mustard","relish","ham"]
-- Just "ham"
--
-- @
-- 'minimumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumByOf' 'folded' cmp
-- @
--
-- @
-- 'minimumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
minimumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then y else x
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- >>> findOf each even (1,3,4,6)
-- Just 4
--
-- >>> findOf folded even [1,3,5,7]
-- Nothing
--
-- @
-- 'findOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- @
--
-- @
-- 'Data.Foldable.find' ≡ 'findOf' 'folded'
-- 'ifindOf' l ≡ 'findOf' l '.' 'Indexed'
-- @
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- 'findOf' :: 'Getting' ('Endo' ('Maybe' a)) s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' l p = 'foldrOf' l (\a y -> if p a then 'Just' a else y) 'Nothing'
-- @
findOf :: Conjoined p => Accessing p (Endo (Maybe a)) s a -> p a Bool -> s -> Maybe a
findOf l p = foldrOf l (cotabulate $ \wa y -> if corep p wa then Just (extract wa) else y) Nothing
{-# INLINE findOf #-}

-- | A variant of 'foldrOf' that has no base case and thus may only be applied
-- to lenses and structures such that the 'Lens' views at least one element of
-- the structure.
--
-- >>> foldr1Of each (+) (1,2,3,4)
-- 10
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- 'Data.Foldable.foldr1' ≡ 'foldr1Of' 'folded'
-- @
--
-- @
-- 'foldr1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of :: Getting (Endo (Maybe a)) s a -> (a -> a -> a) -> s -> a
foldr1Of l f xs = fromMaybe (error "foldr1Of: empty structure")
                            (foldrOf l mf Nothing xs) where
  mf x my = Just $ case my of
    Nothing -> x
    Just y -> f x y
{-# INLINE foldr1Of #-}

-- | A variant of 'foldlOf' that has no base case and thus may only be applied to lenses and structures such
-- that the 'Lens' views at least one element of the structure.
--
-- >>> foldl1Of each (+) (1,2,3,4)
-- 10
--
-- @
-- 'foldl1Of' l f ≡ 'Prelude.foldl1' f '.' 'toListOf' l
-- 'Data.Foldable.foldl1' ≡ 'foldl1Of' 'folded'
-- @
--
-- @
-- 'foldl1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of :: Getting (Dual (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
foldl1Of l f xs = fromMaybe (error "foldl1Of: empty structure") (foldlOf l mf Nothing xs) where
  mf mx y = Just $ case mx of
    Nothing -> y
    Just x  -> f x y
{-# INLINE foldl1Of #-}

-- | Strictly fold right over the elements of a structure.
--
-- @
-- 'Data.Foldable.foldr'' ≡ 'foldrOf'' 'folded'
-- @
--
-- @
-- 'foldrOf'' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf' :: Getting (Dual (Endo (Endo r))) s a -> (a -> r -> r) -> r -> s -> r
foldrOf' l f z0 xs = foldlOf l f' (Endo id) xs `appEndo` z0
  where f' (Endo k) x = Endo $ \ z -> k $! f x z
{-# INLINE foldrOf' #-}

-- | Fold over the elements of a structure, associating to the left, but strictly.
--
-- @
-- 'Data.Foldable.foldl'' ≡ 'foldlOf'' 'folded'
-- @
--
-- @
-- 'foldlOf'' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf' :: Getting (Endo (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf' l f z0 xs = foldrOf l f' (Endo id) xs `appEndo` z0
  where f' x (Endo k) = Endo $ \z -> k $! f z x
{-# INLINE foldlOf' #-}

-- | A variant of 'foldrOf'' that has no base case and thus may only be applied
-- to folds and structures such that the fold views at least one element of the
-- structure.
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- @
--
-- @
-- 'foldr1Of'' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of' :: Getting (Dual (Endo (Endo (Maybe a)))) s a -> (a -> a -> a) -> s -> a
foldr1Of' l f xs = fromMaybe (error "foldr1Of': empty structure") (foldrOf' l mf Nothing xs) where
  mf x Nothing = Just $! x
  mf x (Just y) = Just $! f x y
{-# INLINE foldr1Of' #-}

-- | A variant of 'foldlOf'' that has no base case and thus may only be applied
-- to folds and structures such that the fold views at least one element of
-- the structure.
--
-- @
-- 'foldl1Of'' l f ≡ 'Data.List.foldl1'' f '.' 'toListOf' l
-- @
--
-- @
-- 'foldl1Of'' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of' :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
foldl1Of' l f xs = fromMaybe (error "foldl1Of': empty structure") (foldlOf' l mf Nothing xs) where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! f x y
{-# INLINE foldl1Of' #-}

-- | Monadic fold over the elements of a structure, associating to the right,
-- i.e. from right to left.
--
-- @
-- 'Data.Foldable.foldrM' ≡ 'foldrMOf' 'folded'
-- @
--
-- @
-- 'foldrMOf' :: 'Monad' m => 'Getter' s a     -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Fold' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Iso'' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Lens'' s a      -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Traversal'' s a -> (a -> r -> m r) -> r -> s -> m r
-- @
foldrMOf :: Monad m
         => Getting (Dual (Endo (r -> m r))) s a
         -> (a -> r -> m r) -> r -> s -> m r
foldrMOf l f z0 xs = foldlOf l f' return xs z0
  where f' k x z = f x z >>= k
{-# INLINE foldrMOf #-}

-- | Monadic fold over the elements of a structure, associating to the left,
-- i.e. from left to right.
--
-- @
-- 'Data.Foldable.foldlM' ≡ 'foldlMOf' 'folded'
-- @
--
-- @
-- 'foldlMOf' :: 'Monad' m => 'Getter' s a     -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Fold' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Iso'' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Lens'' s a      -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Traversal'' s a -> (r -> a -> m r) -> r -> s -> m r
-- @
foldlMOf :: Monad m
         => Getting (Endo (r -> m r)) s a
         -> (r -> a -> m r) -> r -> s -> m r
foldlMOf l f z0 xs = foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}

-- | Check to see if this 'Fold' or 'Traversal' matches 1 or more entries.
--
-- >>> has (element 0) []
-- False
--
-- >>> has _Left (Left 12)
-- True
--
-- >>> has _Right (Left 12)
-- False
--
-- This will always return 'True' for a 'Lens' or 'Getter'.
--
-- >>> has _1 ("hello","world")
-- True
--
-- @
-- 'has' :: 'Getter' s a     -> s -> 'Bool'
-- 'has' :: 'Fold' s a       -> s -> 'Bool'
-- 'has' :: 'Iso'' s a       -> s -> 'Bool'
-- 'has' :: 'Lens'' s a      -> s -> 'Bool'
-- 'has' :: 'Traversal'' s a -> s -> 'Bool'
-- @
has :: Getting Any s a -> s -> Bool
has l = getAny #. foldMapOf l (\_ -> Any True)
{-# INLINE has #-}



-- | Check to see if this 'Fold' or 'Traversal' has no matches.
--
-- >>> hasn't _Left (Right 12)
-- True
--
-- >>> hasn't _Left (Left 12)
-- False
hasn't :: Getting All s a -> s -> Bool
hasn't l = getAll #. foldMapOf l (\_ -> All False)
{-# INLINE hasn't #-}

------------------------------------------------------------------------------
-- Pre
------------------------------------------------------------------------------

-- | This converts a 'Fold' to a 'IndexPreservingGetter' that returns the first element, if it
-- exists, as a 'Maybe'.
--
-- @
-- 'pre' :: 'Getter' s a           -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Fold' s a             -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Simple' 'Traversal' s a -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Simple' 'Lens' s a      -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Simple' 'Iso' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Simple' 'Prism' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- @
pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
pre l = dimap (getFirst . runAccessor #. l (Accessor #. First #. Just)) coerce
{-# INLINE pre #-}

-- | This converts an 'IndexedFold' to an 'IndexPreservingGetter' that returns the first index
-- and element, if they exist, as a 'Maybe'.
--
-- @
-- 'ipre' :: 'IndexedGetter' i s a             -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedFold' i s a               -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'Simple' ('IndexedTraversal' i) s a -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'Simple' ('IndexedLens' i) s a      -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- @
ipre :: IndexedGetting i (First (i, a)) s a -> IndexPreservingGetter s (Maybe (i, a))
ipre l = dimap (getFirst . runAccessor #. l (Indexed $ \i a -> Accessor (First (Just (i, a))))) coerce
{-# INLINE ipre #-}

------------------------------------------------------------------------------
-- Preview
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also ('^?').
--
-- @
-- 'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'preview' 'folded'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'preview' = 'view' '.' 'pre'
-- @
--
-- @
-- 'preview' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'preview' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'preview' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'preview' :: 'MonadReader' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Traversal'' s a -> m ('Maybe' a)
-- @
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))
{-# INLINE preview #-}

-- | Retrieve the first index and value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also ('^@?').
--
-- @
-- 'ipreview' = 'view' '.' 'ipre'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'ipreview' :: 'IndexedGetter' i s a     -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedFold' i s a       -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedLens'' i s a      -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedTraversal'' i s a -> s -> 'Maybe' (i, a)
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'ipreview' :: 'MonadReader' s m => 'IndexedGetter' s a     -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedFold' s a       -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedLens'' s a      -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedTraversal'' s a -> m ('Maybe' (i, a))
-- @
ipreview :: MonadReader s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. ifoldMapOf l (\i a -> First (Just (i, a))))
{-# INLINE ipreview #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens').
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.

-- @
-- 'previews' = 'views' '.' 'pre'
-- @
--
-- @
-- 'previews' :: 'Getter' s a     -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Fold' s a       -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Lens'' s a      -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Iso'' s a       -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Traversal'' s a -> (a -> r) -> s -> 'Maybe' r
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Monad' transformer stack:
--
-- @
-- 'previews' :: 'MonadReader' s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
previews :: MonadReader s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
previews l f = asks (getFirst . foldMapOf l (First #. Just . f))
{-# INLINE previews #-}

-- | Retrieve a function of the first index and value targeted by an 'IndexedFold' or
-- 'IndexedTraversal' (or 'Just' the result from an 'IndexedGetter' or 'IndexedLens').
-- See also ('^@?').
--
-- @
-- 'ipreviews' = 'views' '.' 'ipre'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'ipreviews' :: 'IndexedGetter' i s a     -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedFold' i s a       -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedLens'' i s a      -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedTraversal'' i s a -> (i -> a -> r) -> s -> 'Maybe' r
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedGetter' i s a     -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedFold' i s a       -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedLens'' i s a      -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedTraversal'' i s a -> (i -> a -> r) -> m ('Maybe' r)
-- @
ipreviews :: MonadReader s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreviews l f = asks (getFirst . ifoldMapOf l (\i -> First #. Just . f i))
{-# INLINE ipreviews #-}

------------------------------------------------------------------------------
-- Preuse
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuse' = 'use' '.' 'pre'
-- @
--
-- @
-- 'preuse' :: 'MonadState' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Traversal'' s a -> m ('Maybe' a)
-- @
preuse :: MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = gets (preview l)
{-# INLINE preuse #-}

-- | Retrieve the first index and value targeted by an 'IndexedFold' or 'IndexedTraversal' (or 'Just' the index 
-- and result from an 'IndexedGetter' or 'IndexedLens') into the current state.
--
-- @
-- 'ipreuse' = 'use' '.' 'ipre'
-- @
--
-- @
-- 'ipreuse' :: 'MonadState' s m => 'IndexedGetter' i s a     -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedFold' i s a       -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedLens'' i s a      -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedTraversal'' i s a -> m ('Maybe' (i, a))
-- @
ipreuse :: MonadState s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreuse l = gets (ipreview l)
{-# INLINE ipreuse #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuses' = 'uses' '.' 'pre'
-- @
--
-- @
-- 'preuses' :: 'MonadState' s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
preuses :: MonadState s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
preuses l f = gets (previews l f)
{-# INLINE preuses #-}

-- | Retrieve a function of the first index and value targeted by an 'IndexedFold' or
-- 'IndexedTraversal' (or a function of 'Just' the index and result from an 'IndexedGetter'
-- or 'IndexedLens') into the current state.
--
-- @
-- 'ipreuses' = 'uses' '.' 'ipre'
-- @
--
-- @
-- 'ipreuses' :: 'MonadState' s m => 'IndexedGetter' i s a     -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedFold' i s a       -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedLens'' i s a      -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedTraversal'' i s a -> (i -> a -> r) -> m ('Maybe' r)
-- @
ipreuses :: MonadState s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreuses l f = gets (ipreviews l f)
{-# INLINE ipreuses #-}

------------------------------------------------------------------------------
-- Profunctors
------------------------------------------------------------------------------


-- | This allows you to 'Control.Traversable.traverse' the elements of a pretty much any 'LensLike' construction in the opposite order.
--
-- This will preserve indexes on 'Indexed' types and will give you the elements of a (finite) 'Fold' or 'Traversal' in the opposite order.
--
-- This has no practical impact on a 'Getter', 'Setter', 'Lens' or 'Iso'.
--
-- /NB:/ To write back through an 'Iso', you want to use 'Control.Lens.Isomorphic.from'.
-- Similarly, to write back through an 'Prism', you want to use 'Control.Lens.Review.re'.
backwards :: (Profunctor p, Profunctor q) => Overloading p q (Backwards f) s t a b -> Overloading p q f s t a b
backwards l f = forwards #. l (Backwards #. f)
{-# INLINE backwards #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Fold an 'IndexedFold' or 'IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the @i@.
--
-- When you don't need access to the index then 'foldMapOf' is more flexible in what it accepts.
--
-- @
-- 'foldMapOf' l ≡ 'ifoldMapOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i s a     -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i s a       -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' ::             'IndexedLens'' i s a      -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedTraversal'' i s a -> (i -> a -> m) -> s -> m
-- @
--
ifoldMapOf :: IndexedGetting i m s a -> (i -> a -> m) -> s -> m
ifoldMapOf l = foldMapOf l .# Indexed
{-# INLINE ifoldMapOf #-}

-- | Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldrOf' is more flexible in what it accepts.
--
-- @
-- 'foldrOf' l ≡ 'ifoldrOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf :: IndexedGetting i (Endo r) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l = foldrOf l .# Indexed
{-# INLINE ifoldrOf #-}

-- | Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldlOf' is more flexible in what it accepts.
--
-- @
-- 'foldlOf' l ≡ 'ifoldlOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i s a     -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedFold' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedLens'' i s a      -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedTraversal'' i s a -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z = (flip appEndo z .# getDual) `rmap` ifoldMapOf l (\i -> Dual #. Endo #. flip (f i))
{-# INLINE ifoldlOf #-}

-- | Return whether or not any element viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'anyOf' is more flexible in what it accepts.
--
-- @
-- 'anyOf' l ≡ 'ianyOf' l '.' 'const'
-- @
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
ianyOf l = anyOf l .# Indexed
{-# INLINE ianyOf #-}

-- | Return whether or not all elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'allOf' is more flexible in what it accepts.
--
-- @
-- 'allOf' l ≡ 'iallOf' l '.' 'const'
-- @
--
-- @
-- 'iallOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
iallOf :: IndexedGetting i All s a -> (i -> a -> Bool) -> s -> Bool
iallOf l = allOf l .# Indexed
{-# INLINE iallOf #-}

-- | Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the @i@, discarding the results.
--
-- When you don't need access to the index then 'traverseOf_' is more flexible in what it accepts.
--
-- @
-- 'traverseOf_' l ≡ 'Control.Lens.Traversal.itraverseOf' l '.' 'const'
-- @
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> (i -> a -> f r) -> s -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l = traverseOf_ l .# Indexed
{-# INLINE itraverseOf_ #-}

-- | Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @
-- 'iforOf_' ≡ 'flip' '.' 'itraverseOf_'
-- @
--
-- When you don't need access to the index then 'forOf_' is more flexible in what it accepts.
--
-- @
-- 'forOf_' l a ≡ 'iforOf_' l a '.' 'const'
-- @
--
-- @
-- 'iforOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> s -> (i -> a -> f r) -> f ()
-- @
iforOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'mapMOf_' is more flexible in what it accepts.
--
-- @
-- 'mapMOf_' l ≡ 'Control.Lens.Setter.imapMOf' l '.' 'const'
-- @
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m r) -> s -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> (i -> a -> m r) -> s -> m ()
imapMOf_ l = mapMOf_ l .# Indexed
{-# INLINE imapMOf_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @
-- 'iforMOf_' ≡ 'flip' '.' 'imapMOf_'
-- @
--
-- When you don't need access to the index then 'forMOf_' is more flexible in what it accepts.
--
-- @
-- 'forMOf_' l a ≡ 'Control.Lens.Traversal.iforMOf' l a '.' 'const'
-- @
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> s -> (i -> a -> m r) -> m ()
-- @
iforMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> s -> (i -> a -> m r) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- | Concatenate the results of a function of the elements of an 'IndexedFold' or 'IndexedTraversal'
-- with access to the index.
--
-- When you don't need access to the index then 'concatMapOf'  is more flexible in what it accepts.
--
-- @
-- 'concatMapOf' l ≡ 'iconcatMapOf' l '.' 'const'
-- 'iconcatMapOf' ≡ 'ifoldMapOf'
-- @
--
-- @
-- 'iconcatMapOf' :: 'IndexedGetter' i s a     -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedFold' i s a       -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedLens'' i s a      -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedTraversal'' i s a -> (i -> a -> [r]) -> s -> [r]
-- @
iconcatMapOf :: IndexedGetting i [r] s a -> (i -> a -> [r]) -> s -> [r]
iconcatMapOf = ifoldMapOf
{-# INLINE iconcatMapOf #-}

-- | The 'findOf' function takes an 'IndexedFold' or 'IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findOf' is more flexible in what it accepts.
--
-- @
-- 'findOf' l ≡ 'ifindOf' l '.' 'const'
-- @
--
-- @
-- 'ifindOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- @
ifindOf :: IndexedGetting i (Endo (Maybe a)) s a -> (i -> a -> Bool) -> s -> Maybe a
ifindOf l = findOf l .# Indexed
{-# INLINE ifindOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrOf'' is more flexible in what it accepts.
--
-- @
-- 'foldrOf'' l ≡ 'ifoldrOf'' l '.' 'const'
-- @
--
-- @
-- 'ifoldrOf'' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf' :: IndexedGetting i (Dual (Endo (r -> r))) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'foldlOf'' is more flexible in what it accepts.
--
-- @
-- 'foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'
-- @
--
-- @
-- 'ifoldlOf'' :: 'IndexedGetter' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedFold' i s a         -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedLens'' i s a        -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedTraversal'' i s a   -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf' :: IndexedGetting i (Endo (r -> r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrMOf' is more flexible in what it accepts.
--
-- @
-- 'foldrMOf' l ≡ 'ifoldrMOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> r -> m r) -> r -> s -> m r
-- @
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (r -> m r))) s a -> (i -> a -> r -> m r) -> r -> s -> m r
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlMOf' is more flexible in what it accepts.
--
-- @
-- 'foldlMOf' l ≡ 'ifoldlMOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> r -> a -> m r) -> r -> s -> m r
-- @
ifoldlMOf :: Monad m => IndexedGetting i (Endo (r -> m r)) s a -> (i -> r -> a -> m r) -> r -> s -> m r
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'toListOf' is more flexible in what it accepts.
--
-- @
-- 'toListOf' l ≡ 'map' 'fst' '.' 'itoListOf' l
-- @
--
-- @
-- 'itoListOf' :: 'IndexedGetter' i s a     -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedFold' i s a       -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedLens'' i s a      -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedTraversal'' i s a -> s -> [(i,a)]
-- @
itoListOf :: IndexedGetting i (Endo [(i,a)]) s a -> s -> [(i,a)]
itoListOf l = ifoldrOf l (\i a -> ((i,a):)) []
{-# INLINE itoListOf #-}

-- | An infix version of 'itoListOf'.

-- @
-- ('^@..') :: s -> 'IndexedGetter' i s a     -> [(i,a)]
-- ('^@..') :: s -> 'IndexedFold' i s a       -> [(i,a)]
-- ('^@..') :: s -> 'IndexedLens'' i s a      -> [(i,a)]
-- ('^@..') :: s -> 'IndexedTraversal'' i s a -> [(i,a)]
-- @
(^@..) :: s -> IndexedGetting i (Endo [(i,a)]) s a -> [(i,a)]
s ^@.. l = ifoldrOf l (\i a -> ((i,a):)) [] s
{-# INLINE (^@..) #-}

-- | Perform a safe 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' or retrieve 'Just' the index and result
-- from an 'IndexedGetter' or 'IndexedLens'.
--
-- When using a 'IndexedTraversal' as a partial 'IndexedLens', or an 'IndexedFold' as a partial 'IndexedGetter' this can be a convenient
-- way to extract the optional value.
--
-- @
-- ('^@?') :: s -> 'IndexedGetter' i s a     -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedFold' i s a       -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedLens'' i s a      -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedTraversal'' i s a -> 'Maybe' (i, a)
-- @
(^@?) :: s -> IndexedGetting i (Endo (Maybe (i, a))) s a -> Maybe (i, a)
s ^@? l = ifoldrOf l (\i x _ -> Just (i,x)) Nothing s
{-# INLINE (^@?) #-}

-- | Perform an *UNSAFE* 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' assuming that it is there.
--
-- @
-- ('^@?!') :: s -> 'IndexedGetter' i s a     -> (i, a)
-- ('^@?!') :: s -> 'IndexedFold' i s a       -> (i, a)
-- ('^@?!') :: s -> 'IndexedLens'' i s a      -> (i, a)
-- ('^@?!') :: s -> 'IndexedTraversal'' i s a -> (i, a)
-- @
(^@?!) :: s -> IndexedGetting i (Endo (i, a)) s a -> (i, a)
s ^@?! l = ifoldrOf l (\i x _ -> (i,x)) (error "(^@?!): empty Fold") s
{-# INLINE (^@?!) #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Filter an 'IndexedFold' or 'IndexedGetter', obtaining an 'IndexedFold'.
--
-- >>> [0,0,0,5,5,5]^..traversed.ifiltered (\i a -> i <= a)
-- [0,5,5,5]
--
-- Compose with 'filtered' to filter another 'IndexedLens', 'IndexedIso', 'IndexedGetter', 'IndexedFold' (or 'IndexedTraversal') with
-- access to both the value and the index.
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
ifiltered :: (Indexable i p, Applicative f) => (i -> a -> Bool) -> Overloading' p (Indexed i) f a a
ifiltered p f = Indexed $ \i a -> if p i a then indexed f i a else pure a
{-# INLINE ifiltered #-}

-- | Obtain an 'IndexedFold' by taking elements from another
-- 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMonadicFold' i m s a -> 'IndexedMonadicFold' i m s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedAction' i m s a      -> 'IndexedMonadicFold' i m s a
-- @
itakingWhile :: (Indexable i p, Profunctor q, Contravariant f, Applicative f)
         => (i -> a -> Bool)
         -> Overloading (Indexed i) q (Accessor (Endo (f s))) s s a a
         -> Overloading p q f s s a a
itakingWhile p l f = (flip appEndo noEffect .# runAccessor) `rmap` l g where
  g = Indexed $ \i a -> Accessor . Endo $ if p i a then (indexed f i a *>) else const noEffect
{-# INLINE itakingWhile #-}

-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMonadicFold' i m s a -> 'IndexedMonadicFold' i m s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedAction' i m s a      -> 'IndexedMonadicFold' i m s a
-- @
--
-- Applying 'idroppingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still allow you to use it as a
-- pseudo-'IndexedTraversal', but if you change the value of the targets to ones where the predicate returns
-- 'True', then you will break the 'Traversal' laws and 'Traversal' fusion will no longer be sound.
idroppingWhile :: (Indexable i p, Profunctor q, Applicative f)
              => (i -> a -> Bool)
              -> Overloading (Indexed i) q (Compose (State Bool) f) s t a a
              -> Overloading p q f s t a a
idroppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = Indexed $ \ i a -> Compose $ state $ \b -> let
      b' = b && p i a
    in (if b' then pure a else indexed f i a, b')
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Deprecated
------------------------------------------------------------------------------

-- | A deprecated alias for 'firstOf'.
headOf :: Getting (First a) s a -> s -> Maybe a
headOf l = getFirst #. foldMapOf l (First #. Just)
{-# INLINE headOf #-}
{-# DEPRECATED headOf "`headOf' will be removed after GHC 7.8 is released. (Use `preview' or `firstOf')" #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}
