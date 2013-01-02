{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
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
  , IndexedFold

  -- * Getting Started
  , (^..)
  , (^?)
  , (^?!)
  , preview, previews
  , preuse, preuses
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
  ) where

import Control.Applicative as Applicative
import Control.Applicative.Backwards
import Control.Comonad
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable as Foldable
import Data.Functor.Compose
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Representable

-- $setup
-- >>> import Control.Lens
-- >>> import Data.List.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> import Control.DeepSeq (NFData (..), force)
-- >>> import Control.Exception (evaluate)
-- >>> import Data.Maybe (fromMaybe)
-- >>> import System.Timeout (timeout)
-- >>> let timingOut :: NFData a => a -> IO a; timingOut = fmap (fromMaybe (error "timeout")) . timeout (5*10^6) . evaluate . force

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!

--------------------------
-- Folds
--------------------------

-- | Obtain a 'Fold' by lifting an operation that returns a foldable result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
--
-- >>> [1,2,3,4]^..folding tail
-- [2,3,4]
folding :: (Foldable f, Applicative g, Gettable g) => (s -> f a) -> LensLike g s t a b
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

-- | Fold by repeating the input forever.
--
-- @'repeat' ≡ 'toListOf' 'repeated'@
--
-- >>> timingOut $ 5^..taking 20 repeated
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
repeated :: Fold a a
repeated f a = as where as = f a *> as
{-# INLINE repeated #-}

-- | A fold that replicates its input @n@ times.
--
-- @'replicate' n ≡ 'toListOf' ('replicated' n)@
--
-- >>> 5^..replicated 20
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
replicated :: Int -> Fold a a
replicated n0 f a = go n0 where
  m = f a
  go 0 = noEffect
  go n = m *> go (n - 1)
{-# INLINE replicated #-}

-- | Transform a fold into a fold that loops over its elements over and over.
--
-- >>> timingOut $ [1,2,3]^..taking 7 (cycled traverse)
-- [1,2,3,1,2,3,1]
cycled :: (Applicative f, Gettable f) => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a *> as
{-# INLINE cycled #-}

-- | Build a fold that unfolds its values from a seed.
--
-- @'Prelude.unfoldr' ≡ 'toListOf' . 'unfolded'@
--
-- >>> 10^..unfolded (\b -> if b == 0 then Nothing else Just (b, b-1))
-- [10,9,8,7,6,5,4,3,2,1]
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

-- | Obtain a 'Fold' that can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal')
--
-- Note: This is /not/ a legal 'Traversal', unless you are very careful not to invalidate the predicate on the target.
--
-- As a counter example, consider that given @evens = 'filtered' 'even'@ the second 'Traversal' law is violated:
--
-- @'over' evens 'succ' '.' 'over' evens 'succ' /= 'over' evens ('succ' '.' 'succ')@
--
-- So, in order for this to qualify as a legal 'Traversal' you can only use it for actions that preserve the result of the predicate!
--
-- >>> [1..10]^..folded.filtered even
-- [2,4,6,8,10]
--
-- This will preserve an index if it is present.
filtered :: (RepresentableProfunctor p, Comonad (Rep p), Applicative f) => (a -> Bool) -> Overloaded' p f a a
filtered p f = tabulatePro $ \ wa -> let a = extract wa in if p a then indexPro f wa else pure a
{-# INLINE filtered #-}

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @'takeWhile' p ≡ 'toListOf' ('takingWhile' p 'folded')@
--
-- >>> timingOut $ toListOf (takingWhile (<=3) folded) [1..]
-- [1,2,3]
--
-- @
-- 'takingWhile' :: (a -> 'Bool') -> 'Fold' s a                             -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Getter' s a                           -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                       -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Lens'' s a                            -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Prism'' s a                           -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Iso'' s a                             -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Action' m s a                         -> 'MonadicFold' m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MonadicFold' m s a                    -> 'MonadicFold' m s a
--
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a              -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a                   -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                    -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a                  -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedAction' i m s a                -> 'IndexedMonadicFold' i m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMonadicFold' i m s a           -> 'IndexedMonadicFold' i m s a
--
-- 'takingWhile' :: (a -> 'Bool') -> 'MeasuredTraversal'' u s a             -> 'MeasuredFold' u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MeasuredLens'' u s a                  -> 'MeasuredFold' u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MeasuredFold' u s a                   -> 'MeasuredFold' u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MeasuredGetter' u s a                 -> 'MeasuredFold' u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MeasuredAction' u m s a               -> 'MeasuredMonadicFold' u m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'MeasuredMonadicFold' u m s a          -> 'MeasuredMonadicFold' u m s a
--
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexPreservingTraversal'' s a        -> 'IndexPreservingFold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexPreservingLens'' s a             -> 'IndexPreservingFold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexPreservingFold' s a              -> 'IndexPreservingFold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexPreservingGetter' s a            -> 'IndexPreservingFold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexPreservingAction' m s a          -> 'IndexPreservingMonadicFold' m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexPreservingMonadicFold' m s a     -> 'IndexPreservingMonadicFold' m s a
--
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredTraversal'' i u s a    -> 'IndexedMeasuredFold' i u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredLens'' i u s a         -> 'IndexedMeasuredFold' i u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredFold' i u s a          -> 'IndexedMeasuredFold' i u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredGetter' i u s a        -> 'IndexedMeasuredFold' i u s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredAction' i u m s a      -> 'IndexedMeasuredMonadicFold' i u m s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredMonadicFold' i u m s a -> 'IndexedMeasuredMonadicFold' i u m s a
-- @
takingWhile :: (RepresentableProfunctor p, Profunctor q, Comonad (Rep p), Applicative f, Gettable f)
         => (a -> Bool)
         -> Overloading p q (Accessor (Endo (f s))) s s a a
         -> Overloading p q f s s a a
takingWhile p l f = (flip appEndo noEffect .# runAccessor) `rmap` l g where
  g = tabulatePro $ \wa -> Accessor . Endo $
    if p (extract wa) then (indexPro f wa *>) else const noEffect
{-# INLINE takingWhile #-}

-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @'dropWhile' p ≡ 'toListOf' ('droppingWhile' p 'folded')@
--
-- >>> toListOf (droppingWhile (<=3) folded) [1..6]
-- [4,5,6]
--
-- >>> toListOf (droppingWhile (<=3) folded) [1,6,1]
-- [6,1]
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'Fold' s a                   -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Getter' s a                 -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Traversal'' s a             -> 'Fold' s a          -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Lens'' s a                  -> 'Fold' s a          -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Prism'' s a                 -> 'Fold' s a          -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Iso'' s a                   -> 'Fold' s a          -- see notes
--
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingTraversal'' s a    -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingLens'' s a         -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingGetter' s a        -> 'IndexPreservingFold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingFold' s a          -> 'IndexPreservingFold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingAction' m s a      -> 'IndexPreservingFold' m s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingMonadicFold' m s a -> 'IndexPreservingMonadicFold' m s a
--
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedAction' i m s a      -> 'IndexedFold' i m s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMonadicFold' i m s a -> 'IndexedMonadicFold' i m s a
--
-- 'droppingWhile' :: (a -> 'Bool') -> 'MeasuredTraversal'' u s a    -> 'MeasuredFold' u s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'MeasuredLens'' u s a         -> 'MeasuredFold' u s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'MeasuredGetter' u s a        -> 'MeasuredFold' u s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'MeasuredFold' u s a          -> 'MeasuredFold' u s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'MeasuredAction' u m s a      -> 'MeasuredFold' u m s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'MeasuredMonadicFold' u m s a -> 'MeasuredMonadicFold' u m s a
--
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredTraversal'' i u s a    -> 'IndexedMeasuredFold' i u s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredLens'' i u s a         -> 'IndexedMeasuredFold' i u s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredGetter' i u s a        -> 'IndexedMeasuredFold' i u s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredFold' i u s a          -> 'IndexedMeasuredFold' i u s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredAction' i u m s a      -> 'IndexedMeasuredFold' i u m s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedMeasuredMonadicFold' i u m s a -> 'IndexedMeasuredMonadicFold' i u m s a
-- @
--
-- Note: Many uses of this combinator will yield something that meets the types, but not the laws of a valid
-- 'Traversal' or 'IndexedTraversal'. The 'Traversal' and 'IndexedTraversal' laws are only satisfied if the
-- new values you assign also pass the predicate! Otherwise subsequent traversals will visit fewer elements
-- and 'Traversal' fusion is not sound.
droppingWhile :: (RepresentableProfunctor p, Comonad (Rep p), Profunctor q, Applicative f)
              => (a -> Bool)
              -> Overloading p q (Compose (State Bool) f) s t a a
              -> Overloading p q f s t a a
droppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = tabulatePro $ \wa -> Compose $ state $ \b -> let
      a = extract wa
      b' = b && p a
    in (if b' then pure a else indexPro f wa, b')
{-# INLINE droppingWhile #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- |
-- @'Data.Foldable.foldMap' = 'foldMapOf' 'folded'@
--
-- @
-- 'foldMapOf' ≡ 'views'
-- 'ifoldMapOf' l = 'foldMapOf' l . 'Indexed'
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
-- 'foldMapOf' :: 'Getting' r s t a b -> (a -> r) -> s -> r
foldMapOf :: (Profunctor p, Profunctor q) => Overloading p q (Accessor r) s t a b -> p a r -> q s r
foldMapOf l f = runAccessor `rmap` l (rmap Accessor f)
{-# INLINE foldMapOf #-}

-- |
-- @'Data.Foldable.fold' = 'foldOf' 'folded'@
--
-- @'foldOf' ≡ 'view'@
--
-- @
-- 'foldOf' ::             'Getter' s m     -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Fold' s m       -> s -> m
-- 'foldOf' ::             'Lens'' s m      -> s -> m
-- 'foldOf' ::             'Iso'' s m       -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Traversal'' s m -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Prism'' s m     -> s -> m
-- @
--
-- @
-- 'foldOf' :: Getting a s t a b -> s -> a
-- @
foldOf :: Profunctor q => Overloading (->) q (Accessor a) s t a b -> q s a
foldOf l = runAccessor `rmap` l Accessor
{-# INLINE foldOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @'Data.Foldable.foldr' ≡ 'foldrOf' 'folded'@
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
-- @'ifoldrOf' l ≡ 'foldrOf' l '.' 'Indexed'@
--
-- @'foldrOf' :: 'Getting' ('Endo' r) s t a b -> (a -> r -> r) -> r -> s -> r@
foldrOf :: (Profunctor p, Profunctor q) => Overloading p q (Accessor (Endo r)) s t a b -> p a (r -> r) -> r -> q s r
foldrOf l f z = flip appEndo z `rmap` foldMapOf l (rmap Endo f)
{-# INLINE foldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @'Data.Foldable.foldl' ≡ 'foldlOf' 'folded'@
--
-- @
-- 'foldlOf' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Prism'' s a     -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf :: Profunctor q => Overloading (->) q (Accessor (Dual (Endo r))) s t a b -> (r -> a -> r) -> r -> q s r
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
toListOf :: Getting (Endo [a]) s t a b -> s -> [a]
toListOf l = foldrOf l (:) []
-- toListOf l = foldMapOf l return
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
-- ('^..') :: s -> 'Getter' s a     -> [a]
-- ('^..') :: s -> 'Fold' s a       -> [a]
-- ('^..') :: s -> 'Lens'' s a      -> [a]
-- ('^..') :: s -> 'Iso'' s a       -> [a]
-- ('^..') :: s -> 'Traversal'' s a -> [a]
-- ('^..') :: s -> 'Prism'' s a     -> [a]
-- @
(^..) :: s -> Getting (Endo [a]) s t a b -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

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
-- 'andOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'andOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'andOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'andOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
andOf :: Getting All s t Bool b -> s -> Bool
andOf l = getAll `rmap` foldMapOf l All
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
-- 'orOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'orOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'orOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'orOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
orOf :: Getting Any s t Bool b -> s -> Bool
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
-- @'Data.Foldable.any' ≡ 'anyOf' 'folded'@
--
-- @'ianyOf' l ≡ 'allOf' l . 'Indexed'@
--
-- @
-- 'anyOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
anyOf :: (Profunctor p, Profunctor q) => Overloading p q (Accessor Any) s t a b -> p a Bool -> q s Bool
anyOf l f = getAny `rmap` foldMapOf l (rmap Any f)
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
-- @'iallOf' l = 'allOf' l . 'Indexed'@
--
-- @
-- 'allOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
allOf :: (Profunctor p, Profunctor q) => Overloading p q (Accessor All) s t a b -> p a Bool -> q s Bool
allOf l f = getAll `rmap` foldMapOf l (rmap All f)
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
-- 'productOf' ::          'Getter' s a     -> s -> a
-- 'productOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'productOf' ::          'Lens'' s a      -> s -> a
-- 'productOf' ::          'Iso'' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'productOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
productOf :: Getting (Product a) s t a b -> s -> a
productOf l = getProduct #. foldMapOf l Product
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
-- 'sumOf' ::          'Getter' s a     -> s -> a
-- 'sumOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'sumOf' ::          'Lens'' s a      -> s -> a
-- 'sumOf' ::          'Iso'' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'sumOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
sumOf :: Getting (Sum a) s t a b -> s -> a
sumOf l = getSum #. foldMapOf l Sum
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
-- @'itraverseOf_' l ≡ 'traverseOf_' l . 'Indexed'@
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
traverseOf_ :: (Profunctor p, Profunctor q, Functor f) => Overloading p q (Accessor (Traversed f)) s t a b -> p a (f r) -> q s (f ())
traverseOf_ l f = getTraversed `rmap` foldMapOf l (rmap (Traversed . void) f)
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
-- @'iforOf_' l s ≡ 'forOf_' l s . 'Indexed'@
--
-- @
-- 'forOf_' :: 'Functor' f     => 'Getter' s a     -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Fold' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Lens'' s a      -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Iso'' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Traversal'' s a -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Prism'' s a     -> s -> (a -> f r) -> f ()
-- @
forOf_ :: (Profunctor p, Functor f) => Overloading p (->) (Accessor (Traversed f)) s t a b -> s -> p a (f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in observed by a 'Fold' on a structure from left to right, ignoring the results.
--
-- @'sequenceA_' ≡ 'sequenceAOf_' 'folded'@
--
-- @
-- 'sequenceAOf_' :: 'Functor' f     => 'Getter' s (f a)     -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Fold' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Lens'' s (f a)      -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Iso'' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Traversal'' s (f a) -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Prism'' s (f a)     -> s -> f ()
-- @
sequenceAOf_ :: Functor f => Getting (Traversed f) s t (f a) b -> s -> f ()
sequenceAOf_ l = getTraversed #. foldMapOf l (Traversed #. void)
{-# INLINE sequenceAOf_ #-}

-- | Map each target of a 'Fold' on a structure to a monadic action, evaluate these actions from left to right, and ignore the results.
--
-- @'Data.Foldable.mapM_' ≡ 'mapMOf_' 'folded'@
--
-- @
-- 'mapMOf_' :: 'Monad' m => 'Getter' s a     -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Fold' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Lens'' s a      -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Iso'' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Traversal'' s a -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Prism'' s a     -> (a -> m r) -> s -> m ()
-- @
mapMOf_ :: (Profunctor p, Profunctor q, Monad m) => Overloading p q (Accessor (Sequenced m)) s t a b -> p a (m r) -> q s (m ())
mapMOf_ l f = getSequenced `rmap` foldMapOf l (rmap (Sequenced #. liftM skip) f)
{-# INLINE mapMOf_ #-}

-- | 'forMOf_' is 'mapMOf_' with two of its arguments flipped.
--
-- @'Data.Foldable.forM_' ≡ 'forMOf_' 'folded'@
--
-- @
-- 'forMOf_' :: 'Monad' m => 'Getter' s a     -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Fold' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Lens'' s a      -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Iso'' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Traversal'' s a -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Prism'' s a     -> s -> (a -> m r) -> m ()
-- @
forMOf_ :: (Profunctor p, Monad m) => Overloading p (->) (Accessor (Sequenced m)) s t a b -> s -> p a (m r) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- | Evaluate each monadic action referenced by a 'Fold' on the structure from left to right, and ignore the results.
--
-- @'Data.Foldable.sequence_' ≡ 'sequenceOf_' 'folded'@
--
-- @
-- 'sequenceOf_' :: 'Monad' m => 'Getter' s (m a)     -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Fold' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Lens'' s (m a)      -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Iso'' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Traversal'' s (m a) -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Prism'' s (m a)     -> s -> m ()
-- @
sequenceOf_ :: Monad m => Getting (Sequenced m) s t (m a) b -> s -> m ()
sequenceOf_ l = getSequenced #. foldMapOf l (Sequenced #. liftM skip)
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- @'asum' ≡ 'asumOf' 'folded'@
--
-- @
-- 'asumOf' :: 'Alternative' f => 'Getter' s a     -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Fold' s a       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Lens'' s a      -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Iso'' s a       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Traversal'' s a -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Prism'' s a     -> s -> f a
-- @
asumOf :: Alternative f => Getting (Endo (f a)) s t (f a) b -> s -> f a
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- @'msum' ≡ 'msumOf' 'folded'@
--
-- @
-- 'msumOf' :: 'MonadPlus' m => 'Getter' s a     -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Fold' s a       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Lens'' s a      -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Iso'' s a       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Traversal'' s a -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Prism'' s a     -> s -> m a
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
-- 'elemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
elemOf :: Eq a => Getting Any s t a b -> a -> s -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- @'notElem' ≡ 'notElemOf' 'folded'@
--
-- @
-- 'notElemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
notElemOf :: Eq a => Getting All s t a b -> a -> s -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- | Map a function over all the targets of a 'Fold' of a container and concatenate the resulting lists.
--
-- @'concatMap' ≡ 'concatMapOf' 'folded'@
--
-- @
-- 'concatMapOf' :: 'Getter' s a     -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Fold' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Lens'' s a      -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Iso'' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Traversal'' s a -> (a -> [r]) -> s -> [r]
-- @
concatMapOf :: (Profunctor p, Profunctor q) => Overloading p q (Accessor [r]) s t a b -> p a [r] -> q s [r]
concatMapOf l ces = runAccessor `rmap` l (rmap Accessor ces)
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
-- 'lengthOf' :: 'Getter' s a     -> s -> 'Int'
-- 'lengthOf' :: 'Fold' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Lens'' s a      -> s -> 'Int'
-- 'lengthOf' :: 'Iso'' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Traversal'' s a -> s -> 'Int'
-- @
lengthOf :: Getting (Sum Int) s t a b -> s -> Int
lengthOf l = getSum #. foldMapOf l (\_ -> Sum 1)
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- When using a 'Traversal' as a partial 'Lens', or a 'Fold' as a partial 'Getter' this can be a convenient
-- way to extract the optional value.
--
-- @('^?') ≡ 'flip' 'preview'@
--
-- @
-- ('^?') :: s -> 'Getter' s a     -> 'Maybe' a
-- ('^?') :: s -> 'Fold' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Lens'' s a      -> 'Maybe' a
-- ('^?') :: s -> 'Iso'' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Traversal'' s a -> 'Maybe' a
-- @
(^?) :: s -> Getting (Endo (Maybe a)) s t a b -> Maybe a
s ^? l = foldrOf l (\x _ -> Just x) Nothing s
{-# INLINE (^?) #-}

-- | Perform an *UNSAFE* 'head' of a 'Fold' or 'Traversal' assuming that it is there.
--
-- @
-- ('^?!') :: s -> 'Getter' s a     -> a
-- ('^?!') :: s -> 'Fold' s a       -> a
-- ('^?!') :: s -> 'Lens'' s a      -> a
-- ('^?!') :: s -> 'Iso'' s a       -> a
-- ('^?!') :: s -> 'Traversal'' s a -> a
-- @
(^?!) :: s -> Getting (Endo a) s t a b -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

-- | Retrieve the 'First' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- @
-- 'firstOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'firstOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'firstOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
firstOf :: Getting (Endo (Maybe a)) s t a b -> s -> Maybe a
firstOf l = foldrOf l (\x _ -> Just x) Nothing
{-# INLINE firstOf #-}

-- | Retrieve the 'Last' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- @
-- 'lastOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'lastOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'lastOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
lastOf :: Getting (Dual (Endo (Maybe a))) s t a b -> s -> Maybe a
lastOf l = foldlOf l (\_ y -> Just y) Nothing
{-# INLINE lastOf #-}

-- |
-- Returns 'True' if this 'Fold' or 'Traversal' has no targets in the given container.
--
-- Note: 'nullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'False'
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
-- 'nullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'nullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'nullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
nullOf :: Getting All s t a b -> s -> Bool
nullOf l = getAll #. foldMapOf l (\_ -> All False)
{-# INLINE nullOf #-}


-- |
-- Returns 'True' if this 'Fold' or 'Traversal' has any targets in the given container.
--
-- Note: 'notNullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'True'
--
-- @'null' ≡ 'notNullOf' 'folded'@
--
-- This may be rather inefficient compared to the @'not' . 'null'@ check of many containers.
--
-- >>> notNullOf _1 (1,2)
-- True
--
-- @'notNullOf' ('folded' '.' '_1' '.' 'folded') :: 'Foldable' f => f (g a, b) -> 'Bool'@
--
-- @
-- 'notNullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'notNullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'notNullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
notNullOf :: Getting Any s t a b -> s -> Bool
notNullOf l = getAny #. foldMapOf l (\_ -> Any True)
{-# INLINE notNullOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold' or 'Traversal'
--
-- Note: maximumOf on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- @'maximum' ≡ 'fromMaybe' ('error' \"empty\") '.' 'maximumOf' 'folded'@
--
-- @
-- 'maximumOf' ::          'Getter' s a     -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'maximumOf' ::          'Iso'' s a       -> s -> 'Maybe' a
-- 'maximumOf' ::          'Lens'' s a      -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
maximumOf :: Getting (Max a) s t a b -> s -> Maybe a
maximumOf l = getMax . foldMapOf l Max
{-# INLINE maximumOf #-}

-- |
-- Obtain the minimum element (if any) targeted by a 'Fold' or 'Traversal'
--
-- Note: minimumOf on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- @'minimum' ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumOf' 'folded'@
--
-- @
-- 'minimumOf' ::          'Getter' s a     -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'minimumOf' ::          'Iso'' s a       -> s -> 'Maybe' a
-- 'minimumOf' ::          'Lens'' s a      -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
minimumOf :: Getting (Min a) s t a b -> s -> Maybe a
minimumOf l = getMin . foldMapOf l Min
{-# INLINE minimumOf #-}

-- |
-- Obtain the maximum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso',
-- or 'Getter' according to a user supplied ordering.
--
-- @'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumByOf' 'folded' cmp@
--
-- @
-- 'maximumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
maximumByOf :: Getting (Endo (Maybe a)) s t a b -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then a else b)
{-# INLINE maximumByOf #-}

-- |
-- Obtain the minimum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso'
-- or 'Getter' according to a user supplied ordering.
--
-- @'minimumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumByOf' 'folded' cmp@
--
-- @
-- 'minimumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
minimumByOf :: Getting (Endo (Maybe a)) s t a b -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf l cmp = foldrOf l step Nothing where
  step a Nothing  = Just a
  step a (Just b) = Just (if cmp a b == GT then b else a)
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- @
-- 'findOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- @
--
-- @'ifindOf' l = 'findOf' l . 'Indexed'@
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- findOf :: Getting (Endo (Maybe a)) s t a b -> (a -> Bool) -> s -> Maybe a
-- findOf l p = foldrOf l (\a y -> if p a then Just a else y) Nothing
-- @
findOf :: (RepresentableProfunctor p, Comonad (Rep p)) => Overloading p (->) (Accessor (Endo (Maybe a))) s t a b -> p a Bool -> s -> Maybe a
findOf l p = foldrOf l (tabulatePro $ \wa y -> if indexPro p wa then Just (extract wa) else y) Nothing
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
-- 'foldr1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of :: Getting (Endo (Maybe a)) s t a b -> (a -> a -> a) -> s -> a
foldr1Of l f xs = fromMaybe (error "foldr1Of: empty structure")
                            (foldrOf l mf Nothing xs) where
  mf x Nothing = Just x
  mf x (Just y) = Just (f x y)
{-# INLINE foldr1Of #-}

-- | A variant of 'foldlOf' that has no base case and thus may only be applied to lenses and structures such
-- that the lens views at least one element of the structure.
--
-- @
-- 'foldl1Of' l f ≡ 'Prelude.foldl1Of' l f . 'toList'
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
-- 'foldrOf'' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
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
-- 'foldlOf'' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
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
-- 'foldrMOf' :: 'Monad' m => 'Getter' s a     -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Fold' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Iso'' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Lens'' s a      -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Traversal'' s a -> (a -> r -> m r) -> r -> s -> m r
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
-- 'foldlMOf' :: 'Monad' m => 'Getter' s a     -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Fold' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Iso'' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Lens'' s a      -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Traversal'' s a -> (r -> a -> m r) -> r -> s -> m r
-- @
foldlMOf :: Monad m
         => Getting (Endo (r -> m r)) s t a b
         -> (r -> a -> m r) -> r -> s -> m r
foldlMOf l f z0 xs = foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}

-- | Check to see if this 'Fold' or 'Traversal' matches 1 or more entries
--
-- >>> has (element 0) []
-- False
--
-- >>> has _left (Left 12)
-- True
--
-- >>> has _right (Left 12)
-- False
--
-- This will always return True for a 'Lens' or 'Getter'
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
has :: Getting Any s t a b -> s -> Bool
has l = getAny #. views l (\_ -> Any True)
{-# INLINE has #-}

-- | Check to see if this 'Fold' or 'Traversal' has no matches.
--
-- >>> hasn't _left (Right 12)
-- True
--
-- >>> hasn't _left (Left 12)
-- False
hasn't :: Getting All s t a b -> s -> Bool
hasn't l = getAll #. views l (\_ -> All False)
{-# INLINE hasn't #-}

------------------------------------------------------------------------------
-- Preview
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also ('^?').
--
-- @'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'preview' 'folded'@
--
-- This is usually applied in the reader monad @(->) s@.
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
-- a monad transformer stack:
--
-- @
-- 'preview' :: MonadReader s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preview' :: MonadReader s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preview' :: MonadReader s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preview' :: MonadReader s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preview' :: MonadReader s m => 'Traversal'' s a -> m ('Maybe' a)
-- @
preview :: MonadReader s m => Getting (Endo (Maybe a)) s t a b -> m (Maybe a)
preview l = asks (foldrOf l (\x _ -> Just x) Nothing)
{-# INLINE preview #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens').
--
-- This is usually applied in the reader monad @(->) s@.
--
-- @
-- 'previews' :: 'Getter' s a     -> (a -> r) -> s -> 'Maybe' a
-- 'previews' :: 'Fold' s a       -> (a -> r) -> s -> 'Maybe' a
-- 'previews' :: 'Lens'' s a      -> (a -> r) -> s -> 'Maybe' a
-- 'previews' :: 'Iso'' s a       -> (a -> r) -> s -> 'Maybe' a
-- 'previews' :: 'Traversal'' s a -> (a -> r) -> s -> 'Maybe' a
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a monad transformer stack:
--
-- @
-- 'previews' :: MonadReader s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: MonadReader s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: MonadReader s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: MonadReader s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: MonadReader s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
previews :: MonadReader s m => Getting (Endo (Maybe r)) s t a b -> (a -> r) -> m (Maybe r)
previews l f = asks (foldrOf l (\x _ -> Just (f x)) Nothing)
{-# INLINE previews #-}


------------------------------------------------------------------------------
-- Preuse
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuse' :: MonadState s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preuse' :: MonadState s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preuse' :: MonadState s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preuse' :: MonadState s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preuse' :: MonadState s m => 'Traversal'' s a -> m ('Maybe' a)
-- @
preuse :: MonadState s m => Getting (Endo (Maybe a)) s t a b -> m (Maybe a)
preuse l = gets (preview l)
{-# INLINE preuse #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuses' :: MonadState s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: MonadState s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: MonadState s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: MonadState s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: MonadState s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
preuses :: MonadState s m => Getting (Endo (Maybe r)) s t a b -> (a -> r) -> m (Maybe r)
preuses l f = gets (previews l f)
{-# INLINE preuses #-}

------------------------------------------------------------------------------
-- Profunctors
------------------------------------------------------------------------------


-- | This allows you to traverse the elements of a pretty much any lens-like construction in the opposite order.
--
-- This will preserve indexes on indexed types and will give you the elements of a (finite) 'Fold' or 'Traversal' in the opposite order.
--
-- This has no practical impact on a 'Getter', 'Setter', 'Lens' or 'Iso'.
--
-- /NB:/ To write back through an 'Iso', you want to use 'Control.Lens.Isomorphic.from'.
-- Similarly, to write back through an 'Prism', you want to use 'Control.Lens.Prism.remit'.
backwards :: (Profunctor p, Profunctor q) => Overloading p q (Backwards f) s t a b -> Overloading p q f s t a b
backwards l f = rmap forwards (l (rmap Backwards f))
{-# INLINE backwards #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- |
-- Fold an 'IndexedFold' or 'IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the @i@.
--
-- When you don't need access to the index then 'foldMapOf' is more flexible in what it accepts.
--
-- @'foldMapOf' l ≡ 'ifoldMapOf' l '.' 'const'@
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i a s     -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i a s       -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' ::             'IndexedLens'' i a s      -> (i -> s -> m) -> a -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedTraversal'' i a s -> (i -> s -> m) -> a -> m
-- @
ifoldMapOf :: IndexedGetting i m s t a b -> (i -> a -> m) -> s -> m
ifoldMapOf l = foldMapOf l .# Indexed
{-# INLINE ifoldMapOf #-}

-- |
-- Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldrOf' is more flexible in what it accepts.
--
-- @'foldrOf' l ≡ 'ifoldrOf' l '.' 'const'@
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf :: IndexedGetting i (Endo r) s t a b -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l = foldrOf l .# Indexed
{-# INLINE ifoldrOf #-}

-- |
-- Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldlOf' is more flexible in what it accepts.
--
-- @'foldlOf' l ≡ 'ifoldlOf' l '.' 'const'@
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i s a     -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedFold' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedLens'' i s a      -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedTraversal'' i s a -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo r)) s t a b -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z t = appEndo (getDual (ifoldMapOf l (\i -> Dual #. Endo #. flip (f i)) t)) z
{-# INLINE ifoldlOf #-}

-- |
-- Return whether or not any element viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'anyOf' is more flexible in what it accepts.
--
-- @'anyOf' l ≡ 'ianyOf' l '.' 'const'@
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any s t a b -> (i -> a -> Bool) -> s -> Bool
ianyOf l = anyOf l .# Indexed
{-# INLINE ianyOf #-}

-- |
-- Return whether or not all elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'allOf' is more flexible in what it accepts.
--
-- @'allOf' l ≡ 'iallOf' l '.' 'const'@
--
-- @
-- 'iallOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
iallOf :: IndexedGetting i All s t a b -> (i -> a -> Bool) -> s -> Bool
iallOf l = allOf l .# Indexed
{-# INLINE iallOf #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the @i@, discarding the results.
--
-- When you don't need access to the index then 'traverseOf_' is more flexible in what it accepts.
--
-- @'traverseOf_' l ≡ 'Control.Lens.Traversal.itraverseOf' l '.' 'const'@
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> (i -> a -> f r) -> s -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed f) s t a b -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l = traverseOf_ l .# Indexed
{-# INLINE itraverseOf_ #-}

-- |
-- Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @'iforOf_' ≡ 'flip' '.' 'itraverseOf_'@
--
-- When you don't need access to the index then 'forOf_' is more flexible in what it accepts.
--
-- @'forOf_' l a ≡ 'iforOf_' l a '.' 'const'@
--
-- @
-- 'iforOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> s -> (i -> a -> f r) -> f ()
-- @
iforOf_ :: Functor f => IndexedGetting i (Traversed f) s t a b -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'mapMOf_' is more flexible in what it accepts.
--
-- @'mapMOf_' l ≡ 'Control.Lens.Setter.imapMOf' l '.' 'const'@
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m r) -> s -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced m) s t a b -> (i -> a -> m r) -> s -> m ()
imapMOf_ l = mapMOf_ l .# Indexed
{-# INLINE imapMOf_ #-}

-- |
-- Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @'iforMOf_' ≡ 'flip' '.' 'imapMOf_'@
--
-- When you don't need access to the index then 'forMOf_' is more flexible in what it accepts.
--
-- @'forMOf_' l a ≡ 'Control.Lens.Traversal.iforMOf' l a '.' 'const'@
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> s -> (i -> a -> m r) -> m ()
-- @
iforMOf_ :: Monad m => IndexedGetting i (Sequenced m) s t a b -> s -> (i -> a -> m r) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- |
-- Concatenate the results of a function of the elements of an 'IndexedFold' or 'IndexedTraversal'
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
iconcatMapOf :: IndexedGetting i [r] s t a b -> (i -> a -> [r]) -> s -> [r]
iconcatMapOf = ifoldMapOf
{-# INLINE iconcatMapOf #-}

-- | The 'findOf' function takes an 'IndexedFold' or 'IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findOf' is more flexible in what it accepts.
--
-- @'findOf' l ≡ 'ifindOf' l '.' 'const'@
--
-- @
-- 'ifindOf' :: 'IndexedGetter' s a     -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'IndexedFold' s a       -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'IndexedLens'' s a      -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- 'ifindOf' :: 'IndexedTraversal'' s a -> (i -> a -> 'Bool') -> s -> 'Maybe' (i, a)
-- @
ifindOf :: IndexedGetting i (Endo (Maybe a)) s t a b -> (i -> a -> Bool) -> s -> Maybe a
ifindOf l = findOf l .# Indexed
{-# INLINE ifindOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrOf'' is more flexible in what it accepts.
--
-- @'foldrOf'' l ≡ 'ifoldrOf'' l '.' 'const'@
--
-- @
-- 'ifoldrOf'' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf' :: IndexedGetting i (Dual (Endo (r -> r))) s t a b -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'foldlOf'' is more flexible in what it accepts.
--
-- @'foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'@
--
-- @
-- 'ifoldlOf'' :: 'IndexedGetter' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedFold' i s a         -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedLens'' i s a        -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedTraversal'' i s a   -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf' :: IndexedGetting i (Endo (r -> r)) s t a b -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrMOf' is more flexible in what it accepts.
--
-- @'foldrMOf' l ≡ 'ifoldrMOf' l '.' 'const'@
--
-- @
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> r -> m r) -> r -> s -> r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> r -> m r) -> r -> s -> r
-- @
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (r -> m r))) s t a b -> (i -> a -> r -> m r) -> r -> s -> m r
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlMOf' is more flexible in what it accepts.
--
-- @'foldlMOf' l ≡ 'ifoldlMOf' l '.' 'const'@
--
-- @
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> r -> a -> m r) -> r -> s -> r
-- 'ifoldlOf'' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> r -> a -> m r) -> r -> s -> r
-- @
ifoldlMOf :: Monad m => IndexedGetting i (Endo (r -> m r)) s t a b -> (i -> r -> a -> m r) -> r -> s -> m r
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'toListOf' is more flexible in what it accepts.
--
-- @'toListOf' l ≡ 'map' 'fst' '.' 'itoListOf' l@
--
-- @
-- 'itoListOf' :: 'IndexedGetter' i s a     -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedFold' i s a       -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedLens'' i s a      -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedTraversal'' i s a -> s -> [(i,a)]
-- @
itoListOf :: IndexedGetting i (Endo [(i,a)]) s t a b -> s -> [(i,a)]
itoListOf l = ifoldrOf l (\i a -> ((i,a):)) []
{-# INLINE itoListOf #-}

-- | An infix version of 'itoListOf'

-- @
-- ('^@..') :: s -> 'IndexedGetter' i s a     -> [(i,a)]
-- ('^@..') :: s -> 'IndexedFold' i s a       -> [(i,a)]
-- ('^@..') :: s -> 'IndexedLens'' i s a      -> [(i,a)]
-- ('^@..') :: s -> 'IndexedTraversal'' i s a -> [(i,a)]
-- @
(^@..) :: s -> IndexedGetting i (Endo [(i,a)]) s t a b -> [(i,a)]
s ^@.. l = ifoldrOf l (\i a -> ((i,a):)) [] s

-- | Perform a safe 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' or retrieve 'Just' the index and result
-- from an 'IndexedGetter' or 'IndexedLens'.
--
-- When using a 'IndexedTraversal' as a partial 'IndexedLens', or an 'IndexedFold' as a partial 'IndexedGetter' this can be a convenient
-- way to extract the optional value.
--
-- @
-- ('^@?') :: s -> 'IndexedGetter' i s a -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedFold' i s a   -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedLens'' i s a  -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'Iso'' i s a          -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'Traversal'' i s a    -> 'Maybe' (i, a)
-- @
(^@?) :: s -> IndexedGetting i (Endo (Maybe (i, a))) s t a b -> Maybe (i, a)
s ^@? l = ifoldrOf l (\i x _ -> Just (i,x)) Nothing s
{-# INLINE (^@?) #-}

-- | Perform an *UNSAFE* 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' assuming that it is there.
--
-- @
-- ('^@?!') :: s -> 'IndexedGetter' i s a -> (i, a)
-- ('^@?!') :: s -> 'IndexedFold' i s a   -> (i, a)
-- ('^@?!') :: s -> 'Lens'' i s a         -> (i, a)
-- ('^@?!') :: s -> 'Iso'' i s a          -> (i, a)
-- ('^@?!') :: s -> 'Traversal'' i s a    -> (i, a)
-- @
(^@?!) :: s -> IndexedGetting i (Endo (i, a)) s t a b -> (i, a)
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
--
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredFold' i u s a          -> 'IndexedMeasuredFold' i u s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredTraversal'' i u s a    -> 'IndexedMeasuredFold' i u s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredLens'' i u s a         -> 'IndexedMeasuredFold' i u s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredGetter' i u s a        -> 'IndexedMeasuredFold' i u s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredMonadicFold' i u m s a -> 'IndexedMeasuredMonadicFold' i u m s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedMesuredAction' i u m s a      -> 'IndexedMeasuredMonadicFold' i u m s a
-- @
itakingWhile :: (Indexable i p, Profunctor q, Applicative f, Gettable f)
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
--
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredFold' i u s a          -> 'IndexedMeasuredFold' i u s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredTraversal'' i u s a    -> 'IndexedMeasuredFold' i u s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredLens'' i u s a         -> 'IndexedMeasuredFold' i u s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredGetter' i u s a        -> 'IndexedMeasuredFold' i u s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMeasuredMonadicFold' i u m s a -> 'IndexedMeasuredMonadicFold' i u m s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedMesuredAction' i u m s a       -> 'IndexedMeasuredMonadicFold' i u m s a
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

-- | A deprecated alias for 'firstOf'
headOf :: Profunctor q => Overloading (->) q (Accessor (First a)) s t a b -> q s (Maybe a)
headOf l = getFirst `rmap` foldMapOf l (First #. Just)
{-# INLINE headOf #-}
{-# DEPRECATED headOf "`headOf' will be removed in 3.8. (Use `preview' or `firstOf')" #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

