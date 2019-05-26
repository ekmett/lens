{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

#ifndef MIN_VERSION_reflection
#define MIN_VERSION_reflection(x,y,z) 1
#endif

{-# LANGUAGE Trustworthy #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-trustworthy-safe #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Fold
-- Copyright   :  (C) 2012-16 Edward Kmett
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
-- and then decorate it with 'Const' to obtain
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
  , folding, ifolding
  , foldring, ifoldring
  , folded
  , folded64
  , unfolded
  , iterated
  , filtered
  , filteredBy
  , backwards
  , repeated
  , replicated
  , cycled
  , takingWhile
  , droppingWhile
  , worded, lined

  -- ** Folding
  , foldMapOf, foldOf
  , foldrOf, foldlOf
  , toListOf, toNonEmptyOf
  , anyOf, allOf, noneOf
  , andOf, orOf
  , productOf, sumOf
  , traverseOf_, forOf_, sequenceAOf_
  , traverse1Of_, for1Of_, sequence1Of_
  , mapMOf_, forMOf_, sequenceOf_
  , asumOf, msumOf
  , concatMapOf, concatOf
  , elemOf, notElemOf
  , lengthOf
  , nullOf, notNullOf
  , firstOf, first1Of, lastOf, last1Of
  , maximumOf, maximum1Of, minimumOf, minimum1Of
  , maximumByOf, minimumByOf
  , findOf
  , findMOf
  , foldrOf', foldlOf'
  , foldr1Of, foldl1Of
  , foldr1Of', foldl1Of'
  , foldrMOf, foldlMOf
  , lookupOf

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
  , inoneOf
  , itraverseOf_
  , iforOf_
  , imapMOf_
  , iforMOf_
  , iconcatMapOf
  , ifindOf
  , ifindMOf
  , ifoldrOf'
  , ifoldlOf'
  , ifoldrMOf
  , ifoldlMOf
  , itoListOf
  , elemIndexOf
  , elemIndicesOf
  , findIndexOf
  , findIndicesOf

  -- ** Building Indexed Folds
  , ifiltered
  , itakingWhile
  , idroppingWhile

  -- * Internal types
  , Leftmost
  , Rightmost
  , Traversed
  , Sequenced

  -- * Fold with Reified Monoid
  , foldBy
  , foldByOf
  , foldMapBy
  , foldMapByOf
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
import Data.CallStack
import Data.Foldable
import Data.Functor.Apply hiding ((<.))
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
#if MIN_VERSION_reflection(2,1,0)
import Data.Reflection
#endif
import Data.Traversable
import Prelude hiding (foldr)

import qualified Data.Semigroup as Semi
import Data.List.NonEmpty (NonEmpty(..))

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Control.Lens.Extras (is)
-- >>> import Data.Function
-- >>> import Data.List.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> import Control.DeepSeq (NFData (..), force)
-- >>> import Control.Exception (evaluate)
-- >>> import Data.Maybe (fromMaybe)
-- >>> import System.Timeout (timeout)
-- >>> import qualified Data.Map as Map
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let timingOut :: NFData a => a -> IO a; timingOut = fmap (fromMaybe (error "timeout")) . timeout (5*10^6) . evaluate . force

#ifdef HLINT
{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Use curry" #-}
{-# ANN module "HLint: ignore Use fmap" #-}
#endif

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
folding :: Foldable f => (s -> f a) -> Fold s a
folding sfa agb = phantom . traverse_ agb . sfa
{-# INLINE folding #-}

ifolding :: (Foldable f, Indexable i p, Contravariant g, Applicative g) => (s -> f (i, a)) -> Over p g s t a b
ifolding sfa f = phantom . traverse_ (phantom . uncurry (indexed f)) . sfa
{-# INLINE ifolding #-}

-- | Obtain a 'Fold' by lifting 'foldr' like function.
--
-- >>> [1,2,3,4]^..foldring foldr
-- [1,2,3,4]
foldring :: (Contravariant f, Applicative f) => ((a -> f a -> f a) -> f a -> s -> f a) -> LensLike f s t a b
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect
{-# INLINE foldring #-}

-- | Obtain 'FoldWithIndex' by lifting 'ifoldr' like function.
ifoldring :: (Indexable i p, Contravariant f, Applicative f) => ((i -> a -> f a -> f a) -> f a -> s -> f a) -> Over p f s t a b
ifoldring ifr f = phantom . ifr (\i a fa -> indexed f i a *> fa) noEffect
{-# INLINE ifoldring #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
--
-- >>> Just 3^..folded
-- [3]
--
-- >>> Nothing^..folded
-- []
--
-- >>> [(1,2),(3,4)]^..folded.both
-- [1,2,3,4]
folded :: Foldable f => IndexedFold Int (f a) a
folded = conjoined (foldring foldr) (ifoldring ifoldr)
{-# INLINE folded #-}

ifoldr :: Foldable f => (Int -> a -> b -> b) -> b -> f a -> b
ifoldr f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
folded64 :: Foldable f => IndexedFold Int64 (f a) a
folded64 = conjoined (foldring foldr) (ifoldring ifoldr64)
{-# INLINE folded64 #-}

ifoldr64 :: Foldable f => (Int64 -> a -> b -> b) -> b -> f a -> b
ifoldr64 f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr64 #-}

-- | Form a 'Fold1' by repeating the input forever.
--
-- @
-- 'repeat' ≡ 'toListOf' 'repeated'
-- @
--
-- >>> timingOut $ 5^..taking 20 repeated
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
--
-- @
-- 'repeated' :: 'Fold1' a a
-- @
repeated :: Apply f => LensLike' f a a
repeated f a = as where as = f a .> as
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

-- | Transform a non-empty 'Fold' into a 'Fold1' that loops over its elements over and over.
--
-- >>> timingOut $ [1,2,3]^..taking 7 (cycled traverse)
-- [1,2,3,1,2,3,1]
--
-- @
-- 'cycled' :: 'Fold1' s a -> 'Fold1' s a
-- @
cycled :: Apply f => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a .> as
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

-- | @x '^.' 'iterated' f@ returns an infinite 'Fold1' of repeated applications of @f@ to @x@.
--
-- @
-- 'toListOf' ('iterated' f) a ≡ 'iterate' f a
-- @
--
-- @
-- 'iterated' :: (a -> a) -> 'Fold1' a a
-- @
iterated :: Apply f => (a -> a) -> LensLike' f a a
iterated f g a0 = go a0 where
  go a = g a .> go (f a)
{-# INLINE iterated #-}

-- | Obtain a 'Fold' that can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- Note: This is /not/ a legal 'Traversal', unless you are very careful not to invalidate the predicate on the target.
--
-- Note: This is also /not/ a legal 'Prism', unless you are very careful not to inject a value that fails the predicate.
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
filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a
filtered p = dimap (\x -> if p x then Right x else Left x) (either pure id) . right'
{-# INLINE filtered #-}

-- | Obtain a potentially empty 'IndexedTraversal' by taking the first element from another,
-- potentially empty `Fold` and using it as an index.
--
-- The resulting optic can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- >>> [(Just 2, 3), (Nothing, 4)] & mapped . filteredBy (_1 . _Just) <. _2 %@~ (*) :: [(Maybe Int, Int)]
-- [(Just 2,6),(Nothing,4)]
--
-- @
-- 'filteredBy' :: 'Fold' a i -> 'IndexedTraversal'' i a a
-- @
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
filteredBy :: (Indexable i p, Applicative f) => Getting (First i) a i -> p a (f a) -> a -> f a
filteredBy p f val = case val ^? p of
  Nothing -> pure val
  Just witness -> indexed f witness val

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
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- @
--
-- /Note:/ When applied to a 'Traversal', 'takingWhile' yields something that can be used as if it were a 'Traversal', but
-- which is not a 'Traversal' per the laws, unless you are careful to ensure that you do not invalidate the predicate when
-- writing back through it.
takingWhile :: (Conjoined p, Applicative f) => (a -> Bool) -> Over p (TakingWhile p f a a) s t a a -> Over p f s t a a
takingWhile p l pafb = fmap runMagma . traverse (cosieve pafb) . runTakingWhile . l flag where
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
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- @
--
-- Note: Many uses of this combinator will yield something that meets the types, but not the laws of a valid
-- 'Traversal' or 'IndexedTraversal'. The 'Traversal' and 'IndexedTraversal' laws are only satisfied if the
-- new values you assign to the first target also does not pass the predicate! Otherwise subsequent traversals
-- will visit fewer elements and 'Traversal' fusion is not sound.
--
-- So for any traversal @t@ and predicate @p@, @`droppingWhile` p t@ may not be lawful, but
-- @(`Control.Lens.Traversal.dropping` 1 . `droppingWhile` p) t@ is. For example:
--
-- >>> let l  :: Traversal' [Int] Int; l  = droppingWhile (<= 1) traverse
-- >>> let l' :: Traversal' [Int] Int; l' = dropping 1 l
--
-- @l@ is not a lawful setter because @`Control.Lens.Setter.over` l f .
-- `Control.Lens.Setter.over` l g ≢ `Control.Lens.Setter.over` l (f . g)@:
--
-- >>> [1,2,3] & l .~ 0 & l .~ 4
-- [1,0,0]
-- >>> [1,2,3] & l .~ 4
-- [1,4,4]
--
-- @l'@ on the other hand behaves lawfully:
--
-- >>> [1,2,3] & l' .~ 0 & l' .~ 4
-- [1,2,4]
-- >>> [1,2,3] & l' .~ 4
-- [1,2,4]
droppingWhile :: (Conjoined p, Profunctor q, Applicative f)
              => (a -> Bool)
              -> Optical p q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
droppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = cotabulate $ \wa -> Compose $ state $ \b -> let
      a = extract wa
      b' = b && p a
    in (if b' then pure a else cosieve f wa, b')
{-# INLINE droppingWhile #-}

-- | A 'Fold' over the individual 'words' of a 'String'.
--
-- @
-- 'worded' :: 'Fold' 'String' 'String'
-- 'worded' :: 'Traversal'' 'String' 'String'
-- @
--
-- @
-- 'worded' :: 'IndexedFold' 'Int' 'String' 'String'
-- 'worded' :: 'IndexedTraversal'' 'Int' 'String' 'String'
-- @
--
-- Note: This function type-checks as a 'Traversal' but it doesn't satisfy the laws. It's only valid to use it
-- when you don't insert any whitespace characters while traversing, and if your original 'String' contains only
-- isolated space characters (and no other characters that count as space, such as non-breaking spaces).
worded :: Applicative f => IndexedLensLike' Int f String String
worded f = fmap unwords . conjoined traverse (indexing traverse) f . words
{-# INLINE worded #-}

-- | A 'Fold' over the individual 'lines' of a 'String'.
--
-- @
-- 'lined' :: 'Fold' 'String' 'String'
-- 'lined' :: 'Traversal'' 'String' 'String'
-- @
--
-- @
-- 'lined' :: 'IndexedFold' 'Int' 'String' 'String'
-- 'lined' :: 'IndexedTraversal'' 'Int' 'String' 'String'
-- @
--
-- Note: This function type-checks as a 'Traversal' but it doesn't satisfy the laws. It's only valid to use it
-- when you don't insert any newline characters while traversing, and if your original 'String' contains only
-- isolated newline characters.
lined :: Applicative f => IndexedLensLike' Int f String String
lined f = fmap (intercalate "\n") . conjoined traverse (indexing traverse) f . lines
{-# INLINE lined #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- | Map each part of a structure viewed through a 'Lens', 'Getter',
-- 'Fold' or 'Traversal' to a monoid and combine the results.
--
-- >>> foldMapOf (folded . both . _Just) Sum [(Just 21, Just 21)]
-- Sum {getSum = 42}
--
-- @
-- 'Data.Foldable.foldMap' = 'foldMapOf' 'folded'
-- @
--
-- @
-- 'foldMapOf' ≡ 'views'
-- 'ifoldMapOf' l = 'foldMapOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldMapOf' ::                'Getter' s a      -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Fold' s a        -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Semigroup' r => 'Fold1' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::                'Lens'' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::                'Iso'' s a        -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Traversal'' s a  -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Semigroup' r => 'Traversal1'' s a -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Prism'' s a      -> (a -> r) -> s -> r
-- @
--
-- @
-- 'foldMapOf' :: 'Getting' r s a -> (a -> r) -> s -> r
-- @
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

-- | Combine the elements of a structure viewed through a 'Lens', 'Getter',
-- 'Fold' or 'Traversal' using a monoid.
--
-- >>> foldOf (folded.folded) [[Sum 1,Sum 4],[Sum 8, Sum 8],[Sum 21]]
-- Sum {getSum = 42}
--
-- @
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
foldOf l = getConst #. l Const
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
foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
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

-- | Extract a 'NonEmpty' of the targets of 'Fold1'.
--
-- >>> toNonEmptyOf both1 ("hello", "world")
-- "hello" :| ["world"]
--
-- @
-- 'toNonEmptyOf' :: 'Getter' s a      -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Fold1' s a       -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Lens'' s a       -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Iso'' s a        -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Traversal1'' s a -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Prism'' s a      -> s -> NonEmpty a
-- @
toNonEmptyOf :: Getting (NonEmptyDList a) s a -> s -> NonEmpty a
toNonEmptyOf l = flip getNonEmptyDList [] . foldMapOf l (NonEmptyDList #. (:|))

-- | A convenient infix (flipped) version of 'toListOf'.
--
-- >>> [[1,2],[3]]^..id
-- [[[1,2],[3]]]
-- >>> [[1,2],[3]]^..traverse
-- [[1,2],[3]]
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
-- >>> anyOf biplate (== "world") (((),2::Int),"hello",("world",11::Int))
-- True
--
-- @
-- 'Data.Foldable.any' ≡ 'anyOf' 'folded'
-- @
--
-- @
-- 'ianyOf' l ≡ 'anyOf' l '.' 'Indexed'
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
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
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
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll #. foldMapOf l (All #. f)
{-# INLINE allOf #-}

-- | Returns 'True' only if no targets of a 'Fold' satisfy a predicate.
--
-- >>> noneOf each (is _Nothing) (Just 3, Just 4, Just 5)
-- True
-- >>> noneOf (folded.folded) (<10) [[13,99,20],[3,71,42]]
-- False
--
-- @
-- 'inoneOf' l = 'noneOf' l '.' 'Indexed'
-- @
--
-- @
-- 'noneOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
noneOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
noneOf l f = not . anyOf l f
{-# INLINE noneOf #-}

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
--
-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Product' '.' 'foldMapOf'@
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
traverseOf_ :: Functor f => Getting (Traversed r f) s a -> (a -> f r) -> s -> f ()
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
forOf_ :: Functor f => Getting (Traversed r f) s a -> s -> (a -> f r) -> f ()
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

-- | Traverse over all of the targets of a 'Fold1', computing an 'Apply' based answer.
--
-- As long as you have 'Applicative' or 'Functor' effect you are better using 'traverseOf_'.
-- The 'traverse1Of_' is useful only when you have genuine 'Apply' effect.
--
-- >>> traverse1Of_ both1 (\ks -> Map.fromList [ (k, ()) | k <- ks ]) ("abc", "bcd")
-- fromList [('b',()),('c',())]
--
-- @
-- 'traverse1Of_' :: 'Apply' f => 'Fold1' s a -> (a -> f r) -> s -> f ()
-- @
--
-- @since 4.16
traverse1Of_ :: Functor f => Getting (TraversedF r f) s a -> (a -> f r) -> s -> f ()
traverse1Of_ l f = void . getTraversedF #. foldMapOf l (TraversedF #. f)
{-# INLINE traverse1Of_ #-}

-- | See 'forOf_' and 'traverse1Of_'.
--
-- >>> for1Of_ both1 ("abc", "bcd") (\ks -> Map.fromList [ (k, ()) | k <- ks ])
-- fromList [('b',()),('c',())]
--
-- @
-- 'for1Of_' :: 'Apply' f => 'Fold1' s a -> s -> (a -> f r) -> f ()
-- @
--
-- @since 4.16
for1Of_ :: Functor f => Getting (TraversedF r f) s a -> s -> (a -> f r) -> f ()
for1Of_ = flip . traverse1Of_
{-# INLINE for1Of_ #-}

-- | See 'sequenceAOf_' and 'traverse1Of_'.
--
-- @
-- 'sequence1Of_' :: 'Apply' f => 'Fold1' s (f a) -> s -> f ()
-- @
--
-- @since 4.16
sequence1Of_ :: Functor f => Getting (TraversedF a f) s (f a) -> s -> f ()
sequence1Of_ l = void . getTraversedF #. foldMapOf l TraversedF
{-# INLINE sequence1Of_ #-}

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
mapMOf_ :: Monad m => Getting (Sequenced r m) s a -> (a -> m r) -> s -> m ()
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
forMOf_ :: Monad m => Getting (Sequenced r m) s a -> s -> (a -> m r) -> m ()
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
concatMapOf :: Getting [r] s a -> (a -> [r]) -> s -> [r]
concatMapOf l ces = getConst #. l (Const #. ces)
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
concatOf l = getConst #. l Const
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
-- more gracefully with heavily left-biased trees. This is because '^?' works by using the 
-- 'Data.Monoid.First' monoid, which can occasionally cause space leaks.
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
-- This operator works as an infix version of 'preview'. 
--
-- @
-- ('^?') ≡ 'flip' 'preview'
-- @
--
-- It may be helpful to think of '^?' as having one of the following
-- more specialized types:
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
(^?!) :: HasCallStack => s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

-- | Retrieve the 'First' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'preview'@ or @^?'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but does so
-- in a way that builds an intermediate structure, and thus may have worse
-- constant factors. This also means that it can not be used in any 'Control.Monad.Reader.MonadReader',
-- but must instead have 's' passed as its last argument, unlike 'preview'.
--
-- Note: this could been named `headOf`.
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

-- | Retrieve the 'Data.Semigroup.First' entry of a 'Fold1' or 'Traversal1' or the result from a 'Getter' or 'Lens'.
--
-- >>> first1Of traverse1 (1 :| [2..10])
-- 1
--
-- >>> first1Of both1 (1,2)
-- 1
--
-- /Note:/ this is different from '^.'.
--
-- >>> first1Of traverse1 ([1,2] :| [[3,4],[5,6]])
-- [1,2]
--
-- >>> ([1,2] :| [[3,4],[5,6]]) ^. traverse1
-- [1,2,3,4,5,6]
--
-- @
-- 'first1Of' :: 'Getter' s a      -> s -> a
-- 'first1Of' :: 'Fold1' s a       -> s -> a
-- 'first1Of' :: 'Lens'' s a       -> s -> a
-- 'first1Of' :: 'Iso'' s a        -> s -> a
-- 'first1Of' :: 'Traversal1'' s a -> s -> a
-- @
first1Of :: Getting (Semi.First a) s a -> s -> a
first1Of l = Semi.getFirst . foldMapOf l Semi.First

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

-- | Retrieve the 'Data.Semigroup.Last' entry of a 'Fold1' or 'Traversal1' or retrieve the result
-- from a 'Getter' or 'Lens'.o
--
-- >>> last1Of traverse1 (1 :| [2..10])
-- 10
--
-- >>> last1Of both1 (1,2)
-- 2
--
-- @
-- 'last1Of' :: 'Getter' s a      -> s -> 'Maybe' a
-- 'last1Of' :: 'Fold1' s a       -> s -> 'Maybe' a
-- 'last1Of' :: 'Lens'' s a       -> s -> 'Maybe' a
-- 'last1Of' :: 'Iso'' s a        -> s -> 'Maybe' a
-- 'last1Of' :: 'Traversal1'' s a -> s -> 'Maybe' a
-- @
last1Of :: Getting (Semi.Last a) s a -> s -> a
last1Of l = Semi.getLast . foldMapOf l Semi.Last

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
-- 'not' '.' 'null' ≡ 'notNullOf' 'folded'
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

-- | Obtain the maximum element targeted by a 'Fold1' or 'Traversal1'.
--
-- >>> maximum1Of traverse1 (1 :| [2..10])
-- 10
--
-- @
-- 'maximum1Of' :: 'Ord' a => 'Getter' s a      -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Fold1' s a       -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Iso'' s a        -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Lens'' s a       -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Traversal1'' s a -> s -> a
-- @
maximum1Of :: Ord a => Getting (Semi.Max a) s a -> s -> a
maximum1Of l = Semi.getMax . foldMapOf l Semi.Max
{-# INLINE maximum1Of #-}

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

-- | Obtain the minimum element targeted by a 'Fold1' or 'Traversal1'.
--
-- >>> minimum1Of traverse1 (1 :| [2..10])
-- 1
--
-- @
-- 'minimum1Of' :: 'Ord' a => 'Getter' s a      -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Fold1' s a       -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Iso'' s a        -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Lens'' s a       -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Traversal1'' s a -> s -> a
-- @
minimum1Of :: Ord a => Getting (Semi.Min a) s a -> s -> a
minimum1Of l = Semi.getMin . foldMapOf l Semi.Min
{-# INLINE minimum1Of #-}

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
findOf :: Getting (Endo (Maybe a)) s a -> (a -> Bool) -> s -> Maybe a
findOf l f = foldrOf l (\a y -> if f a then Just a else y) Nothing
{-# INLINE findOf #-}

-- | The 'findMOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a monadic predicate and a structure and returns in the monad the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- >>>  findMOf each ( \x -> print ("Checking " ++ show x) >> return (even x)) (1,3,4,6)
-- "Checking 1"
-- "Checking 3"
-- "Checking 4"
-- Just 4
--
-- >>>  findMOf each ( \x -> print ("Checking " ++ show x) >> return (even x)) (1,3,5,7)
-- "Checking 1"
-- "Checking 3"
-- "Checking 5"
-- "Checking 7"
-- Nothing
--
-- @
-- 'findMOf' :: ('Monad' m, 'Getter' s a)     -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Fold' s a)       -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Iso'' s a)       -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Lens'' s a)      -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Traversal'' s a) -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- @
--
-- @
-- 'findMOf' 'folded' :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m (Maybe a)
-- 'ifindMOf' l ≡ 'findMOf' l '.' 'Indexed'
-- @
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- 'findMOf' :: Monad m => 'Getting' ('Endo' (m ('Maybe' a))) s a -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' l p = 'foldrOf' l (\a y -> p a >>= \x -> if x then return ('Just' a) else y) $ return 'Nothing'
-- @
findMOf :: Monad m => Getting (Endo (m (Maybe a))) s a -> (a -> m Bool) -> s -> m (Maybe a)
findMOf l f = foldrOf l (\a y -> f a >>= \r -> if r then return (Just a) else y) $ return Nothing
{-# INLINE findMOf #-}

-- | The 'lookupOf' function takes a 'Fold' (or 'Getter', 'Traversal',
-- 'Lens', 'Iso', etc.), a key, and a structure containing key/value pairs.
-- It returns the first value corresponding to the given key. This function
-- generalizes 'lookup' to work on an arbitrary 'Fold' instead of lists.
--
-- >>> lookupOf folded 4 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'b'
--
-- >>> lookupOf each 2 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'a'
--
-- @
-- 'lookupOf' :: 'Eq' k => 'Fold' s (k,v) -> k -> s -> 'Maybe' v
-- @
lookupOf :: Eq k => Getting (Endo (Maybe v)) s (k,v) -> k -> s -> Maybe v
lookupOf l k = foldrOf l (\(k',v) next -> if k == k' then Just v else next) Nothing
{-# INLINE lookupOf #-}

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
foldr1Of :: HasCallStack => Getting (Endo (Maybe a)) s a -> (a -> a -> a) -> s -> a
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
foldl1Of :: HasCallStack => Getting (Dual (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
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
foldr1Of' :: HasCallStack => Getting (Dual (Endo (Endo (Maybe a)))) s a -> (a -> a -> a) -> s -> a
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
foldl1Of' :: HasCallStack => Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
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
-- 'pre' :: 'Getter' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Fold' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Traversal'' s a -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Lens'' s a      -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Iso'' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Prism'' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- @
pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
pre l = dimap (getFirst . getConst #. l (Const #. First #. Just)) phantom
{-# INLINE pre #-}

-- | This converts an 'IndexedFold' to an 'IndexPreservingGetter' that returns the first index
-- and element, if they exist, as a 'Maybe'.
--
-- @
-- 'ipre' :: 'IndexedGetter' i s a     -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedFold' i s a       -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedTraversal'' i s a -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedLens'' i s a      -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- @
ipre :: IndexedGetting i (First (i, a)) s a -> IndexPreservingGetter s (Maybe (i, a))
ipre l = dimap (getFirst . getConst #. l (Indexed $ \i a -> Const (First (Just (i, a))))) phantom
{-# INLINE ipre #-}

------------------------------------------------------------------------------
-- Preview
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also 'firstOf' and '^?', which are similar with
-- some subtle differences (explained below).
--
-- @
-- 'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'preview' 'folded'
-- @
--
-- @
-- 'preview' = 'view' '.' 'pre'
-- @
-- 
--
-- Unlike '^?', this function uses a 
-- 'Control.Monad.Reader.MonadReader' to read the value to be focused in on.
-- This allows one to pass the value as the last argument by using the 
-- 'Control.Monad.Reader.MonadReader' instance for @(->) s@
-- However, it may also be used as part of some deeply nested transformer stack.
--
-- 'preview' uses a monoidal value to obtain the result.
-- This means that it generally has good performance, but can occasionally cause space leaks
-- or even stack overflows on some data types.
-- There is another function, 'firstOf', which avoids these issues at the cost of
-- a slight constant performance cost and a little less flexibility.
--
-- It may be helpful to think of 'preview' as having one of the following
-- more specialized types:
-- 
-- @
-- 'preview' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'preview' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'preview' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
--
--
-- @
-- 'preview' :: 'MonadReader' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Traversal'' s a -> m ('Maybe' a)
-- 
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
backwards :: (Profunctor p, Profunctor q) => Optical p q (Backwards f) s t a b -> Optical p q f s t a b
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
ifoldMapOf l f = getConst #. l (Const #. Indexed f)
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
ifoldrOf l f z = flip appEndo z . getConst #. l (Const #. Endo #. Indexed f)
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
ianyOf l f = getAny #. getConst #. l (Const #. Any #. Indexed f)
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
iallOf l f = getAll #. getConst #. l (Const #. All #. Indexed f)
{-# INLINE iallOf #-}

-- | Return whether or not none of the elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'noneOf' is more flexible in what it accepts.
--
-- @
-- 'noneOf' l ≡ 'inoneOf' l '.' 'const'
-- @
--
-- @
-- 'inoneOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
inoneOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
inoneOf l f = not . ianyOf l f
{-# INLINE inoneOf #-}

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
itraverseOf_ l f = void . getTraversed #. getConst #. l (Const #. Traversed #. Indexed f)
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
imapMOf_ l f = liftM skip . getSequenced #. getConst #. l (Const #. Sequenced #. Indexed f)
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

-- | The 'ifindOf' function takes an 'IndexedFold' or 'IndexedTraversal', a predicate that is also
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
ifindOf l f = ifoldrOf l (\i a y -> if f i a then Just a else y) Nothing
{-# INLINE ifindOf #-}

-- | The 'ifindMOf' function takes an 'IndexedFold' or 'IndexedTraversal', a monadic predicate that is also
-- supplied the index, a structure and returns in the monad the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findMOf' is more flexible in what it accepts.
--
-- @
-- 'findMOf' l ≡ 'ifindMOf' l '.' 'const'
-- @
--
-- @
-- 'ifindMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- @
ifindMOf :: Monad m => IndexedGetting i (Endo (m (Maybe a))) s a -> (i -> a -> m Bool) -> s -> m (Maybe a)
ifindMOf l f = ifoldrOf l (\i a y -> f i a >>= \r -> if r then return (Just a) else y) $ return Nothing
{-# INLINE ifindMOf #-}

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
-- 'toListOf' l ≡ 'map' 'snd' '.' 'itoListOf' l
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
(^@?!) :: HasCallStack => s -> IndexedGetting i (Endo (i, a)) s a -> (i, a)
s ^@?! l = ifoldrOf l (\i x _ -> (i,x)) (error "(^@?!): empty Fold") s
{-# INLINE (^@?!) #-}

-- | Retrieve the index of the first value targeted by a 'IndexedFold' or 'IndexedTraversal' which is equal to a given value.
--
-- @
-- 'Data.List.elemIndex' ≡ 'elemIndexOf' 'folded'
-- @
--
-- @
-- 'elemIndexOf' :: 'Eq' a => 'IndexedFold' i s a       -> a -> s -> 'Maybe' i
-- 'elemIndexOf' :: 'Eq' a => 'IndexedTraversal'' i s a -> a -> s -> 'Maybe' i
-- @
elemIndexOf :: Eq a => IndexedGetting i (First i) s a -> a -> s -> Maybe i
elemIndexOf l a = findIndexOf l (a ==)
{-# INLINE elemIndexOf #-}

-- | Retrieve the indices of the values targeted by a 'IndexedFold' or 'IndexedTraversal' which are equal to a given value.
--
-- @
-- 'Data.List.elemIndices' ≡ 'elemIndicesOf' 'folded'
-- @
--
-- @
-- 'elemIndicesOf' :: 'Eq' a => 'IndexedFold' i s a       -> a -> s -> [i]
-- 'elemIndicesOf' :: 'Eq' a => 'IndexedTraversal'' i s a -> a -> s -> [i]
-- @
elemIndicesOf :: Eq a => IndexedGetting i (Endo [i]) s a -> a -> s -> [i]
elemIndicesOf l a = findIndicesOf l (a ==)
{-# INLINE elemIndicesOf #-}

-- | Retrieve the index of the first value targeted by a 'IndexedFold' or 'IndexedTraversal' which satisfies a predicate.
--
-- @
-- 'Data.List.findIndex' ≡ 'findIndexOf' 'folded'
-- @
--
-- @
-- 'findIndexOf' :: 'IndexedFold' i s a       -> (a -> 'Bool') -> s -> 'Maybe' i
-- 'findIndexOf' :: 'IndexedTraversal'' i s a -> (a -> 'Bool') -> s -> 'Maybe' i
-- @
findIndexOf :: IndexedGetting i (First i) s a -> (a -> Bool) -> s -> Maybe i
findIndexOf l p = preview (l . filtered p . asIndex)
{-# INLINE findIndexOf #-}

-- | Retrieve the indices of the values targeted by a 'IndexedFold' or 'IndexedTraversal' which satisfy a predicate.
--
-- @
-- 'Data.List.findIndices' ≡ 'findIndicesOf' 'folded'
-- @
--
-- @
-- 'findIndicesOf' :: 'IndexedFold' i s a       -> (a -> 'Bool') -> s -> [i]
-- 'findIndicesOf' :: 'IndexedTraversal'' i s a -> (a -> 'Bool') -> s -> [i]
-- @
findIndicesOf :: IndexedGetting i (Endo [i]) s a -> (a -> Bool) -> s -> [i]
findIndicesOf l p = toListOf (l . filtered p . asIndex)
{-# INLINE findIndicesOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Filter an 'IndexedFold' or 'IndexedGetter', obtaining an 'IndexedFold'.
--
-- >>> [0,0,0,5,5,5]^..traversed.ifiltered (\i a -> i <= a)
-- [0,5,5,5]
--
-- Compose with 'ifiltered' to filter another 'IndexedLens', 'IndexedIso', 'IndexedGetter', 'IndexedFold' (or 'IndexedTraversal') with
-- access to both the value and the index.
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
ifiltered :: (Indexable i p, Applicative f) => (i -> a -> Bool) -> Optical' p (Indexed i) f a a
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
-- @
--
-- Note: Applying 'itakingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still allow you to use it as a
-- pseudo-'IndexedTraversal', but if you change the value of any target to one where the predicate returns
-- 'False', then you will break the 'Traversal' laws and 'Traversal' fusion will no longer be sound.
itakingWhile :: (Indexable i p, Profunctor q, Contravariant f, Applicative f)
         => (i -> a -> Bool)
         -> Optical' (Indexed i) q (Const (Endo (f s))) s a
         -> Optical' p q f s a
itakingWhile p l f = (flip appEndo noEffect .# getConst) `rmap` l g where
  g = Indexed $ \i a -> Const . Endo $ if p i a then (indexed f i a *>) else const noEffect
{-# INLINE itakingWhile #-}

-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- @
--
-- Note: As with `droppingWhile` applying 'idroppingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still
-- allow you to use it as a pseudo-'IndexedTraversal', but if you change the value of the first target to one
-- where the predicate returns 'True', then you will break the 'Traversal' laws and 'Traversal' fusion will
-- no longer be sound.
idroppingWhile :: (Indexable i p, Profunctor q, Applicative f)
              => (i -> a -> Bool)
              -> Optical (Indexed i) q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
idroppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = Indexed $ \ i a -> Compose $ state $ \b -> let
      b' = b && p i a
    in (if b' then pure a else indexed f i a, b')
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

------------------------------------------------------------------------------
-- Folds with Reified Monoid
------------------------------------------------------------------------------

-- | Fold a value using a specified 'Fold' and 'Monoid' operations.
-- This is like 'foldBy' where the 'Foldable' instance can be
-- manually specified.
--
-- @
-- 'foldByOf' 'folded' ≡ 'foldBy'
-- @
--
-- @
-- 'foldByOf' :: 'Getter' s a     -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Fold' s a       -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Lens'' s a      -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Traversal'' s a -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Iso'' s a       -> (a -> a -> a) -> a -> s -> a
-- @
--
-- >>> foldByOf both (++) [] ("hello","world")
-- "helloworld"
foldByOf :: Fold s a -> (a -> a -> a) -> a -> s -> a
foldByOf l f z = reifyMonoid f z (foldMapOf l ReflectedMonoid)

-- | Fold a value using a specified 'Fold' and 'Monoid' operations.
-- This is like 'foldMapBy' where the 'Foldable' instance can be
-- manually specified.
--
-- @
-- 'foldMapByOf' 'folded' ≡ 'foldMapBy'
-- @
--
-- @
-- 'foldMapByOf' :: 'Getter' s a     -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Fold' s a       -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Traversal'' s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Lens'' s a      -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Iso'' s a       -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- @
--
-- >>> foldMapByOf both (+) 0 length ("hello","world")
-- 10
foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
foldMapByOf l f z g = reifyMonoid f z (foldMapOf l (ReflectedMonoid #. g))
