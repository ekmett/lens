{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif
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
-- @'traverse' :: ('Traversable' t, 'Applicative' f) => (a -> f b) -> t a -> f (t b)@
--
-- we monomorphize the contents and result to obtain
--
-- @type 'Traversal' s t a b = forall f. 'Applicative' f => (a -> f b) -> s -> f t@
--
-- While a 'Traversal' isn't quite a 'Fold', it _can_ be used for 'Getting'
-- like a 'Fold', because given a 'Monoid' @m@, we have an 'Applicative'
-- for @('Const' m)@. Everything you know how to do with a 'Traversable'
-- container, you can with with a 'Traversal', and here we provide
-- combinators that generalize the usual 'Traversable' operations.
----------------------------------------------------------------------------
module Control.Lens.Traversal
  (
  -- * Traversals
    Traversal, Traversal'
  , IndexedTraversal, IndexedTraversal'
  , ATraversal, ATraversal'
  , Traversing, Traversing'

  -- * Traversing and Lensing
  , traverseOf, forOf, sequenceAOf
  , mapMOf, forMOf, sequenceOf
  , transposeOf
  , mapAccumLOf, mapAccumROf
  , scanr1Of, scanl1Of

  -- * Monomorphic Traversals
  , cloneTraversal

  -- * Parts and Holes
  , partsOf, partsOf'
  , unsafePartsOf, unsafePartsOf'
  , holesOf
  , singular, unsafeSingular

  -- * Common Traversals
  , Traversable(traverse)
  , both
  , beside
  , taking
  , dropping

  -- * Indexed Traversals

  -- ** Common
  , iwhereOf
  , value
  , ignored
  , TraverseMin(..)
  , TraverseMax(..)
  , traversed
  , traversed64
  , elementOf
  , element
  , elementsOf
  , elements

  -- ** Combinators
  , itraverseOf
  , iforOf
  , imapMOf
  , iforMOf
  , imapAccumROf
  , imapAccumLOf

  -- * Implementation Details
  , Bazaar(..)
  , Bazaar'
  , loci
  ) where

import Control.Applicative            as Applicative
import Control.Applicative.Backwards
import Control.Lens.Combinators
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.State.Class      as State
import Control.Monad.Trans.State.Lazy as Lazy
import Data.Int
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Traversable
import Data.Tuple (swap)

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects a 'Traversal'.
type ATraversal s t a b = LensLike (Bazaar a b) s t a b

-- | @type 'ATraversal'' = 'Simple' 'ATraversal'@
type ATraversal' s a = ATraversal s s a a

-- | When you see this as an argument to a function, it expects
--
--  * it is indexed if @p@ is an instance of 'Indexed i'
--
--  * unindexed if @p@ is @(->)@
--
--  * a 'Traversal' if @f@ is 'Applicative',
--
--  * a 'Getter' if  @f@ is only 'Gettable'
--
--  * a 'Lens' if @p@ is @(->)@ is only a 'Functor'
--
--  * a 'Fold' if 'f' is 'Gettable' and 'Applicative'.
type Traversing p f s t a b = LensLike (BazaarT p f a b) s t a b

-- | @type 'Traversing'' f = 'Simple' ('Traversing' f)@
type Traversing' p f s a = Traversing p f s s a a

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
-- 'traverseOf' :: 'Iso' s t a b       -> (a -> f b) -> s -> f t
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
-- 'forOf' :: 'Iso' s t a b -> s -> (a -> f b) -> f t
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
-- 'sequenceAOf' ::                  'Iso' s t (f b) b       -> s -> f t
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
-- 'mapMOf' ::            'Iso' s t a b       -> (a -> m b) -> s -> m t
-- 'mapMOf' ::            'Lens' s t a b      -> (a -> m b) -> s -> m t
-- 'mapMOf' :: 'Monad' m => 'Traversal' s t a b -> (a -> m b) -> s -> m t
-- @
mapMOf :: LensLike (WrappedMonad m) s t a b -> (a -> m b) -> s -> m t
mapMOf l cmd = unwrapMonad #. l (WrapMonad #. cmd)
{-# INLINE mapMOf #-}

-- | 'forMOf' is a flipped version of 'mapMOf', consistent with the definition of 'forM'.
-- @
-- 'forM' ≡ 'forMOf' 'traverse'
-- 'forMOf' l ≡ 'flip' ('mapMOf' l)
-- @
--
-- @
-- 'forMOf' ::            'Iso' s t a b       -> s -> (a -> m b) -> m t
-- 'forMOf' ::            'Lens' s t a b      -> s -> (a -> m b) -> m t
-- 'forMOf' :: 'Monad' m => 'Traversal' s t a b -> s -> (a -> m b) -> m t
-- @
forMOf :: LensLike (WrappedMonad m) s t a b -> s -> (a -> m b) -> m t
forMOf l a cmd = unwrapMonad (l (WrapMonad #. cmd) a)
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
-- 'sequenceOf' ::            'Iso' s t (m b) b       -> s -> m t
-- 'sequenceOf' ::            'Lens' s t (m b) b      -> s -> m t
-- 'sequenceOf' :: 'Monad' m => 'Traversal' s t (m b) b -> s -> m t
-- @
sequenceOf :: LensLike (WrappedMonad m) s t (m b) b -> s -> m t
sequenceOf l = unwrapMonad #. l WrapMonad
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
transposeOf l = getZipList #. l ZipList
{-# INLINE transposeOf #-}

-- | This generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal'.
--
-- @'mapAccumR' ≡ 'mapAccumROf' 'traverse'@
--
-- 'mapAccumROf' accumulates state from right to left.
--
-- @
-- 'mapAccumROf' :: 'Iso' s t a b       -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumROf' :: 'Lens' s t a b      -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumROf' :: 'Traversal' s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
mapAccumROf :: LensLike (Backwards (Lazy.State acc)) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumROf = mapAccumLOf . backwards
{-# INLINE mapAccumROf #-}

-- | This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'.
--
-- @'mapAccumL' ≡ 'mapAccumLOf' 'traverse'@
--
-- 'mapAccumLOf' accumulates state from left to right.
--
-- @
-- 'mapAccumLOf' :: 'Iso' s t a b       -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' :: 'Lens' s t a b      -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- 'mapAccumLOf' :: 'Traversal' s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
-- @
mapAccumLOf :: LensLike (Lazy.State acc) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
#if MIN_VERSION_mtl(2,1,1)
mapAccumLOf l f acc0 s = swap (Lazy.runState (l (\a -> State.state (\acc -> swap (f acc a))) s) acc0)
#else
mapAccumLOf l f acc0 s = swap (Lazy.runState (l (\a -> do (r,s') <- State.gets (\acc -> swap (f acc a)); State.put s'; return r) s) acc0)
#endif
{-# INLINE mapAccumLOf #-}

-- | This permits the use of 'scanr1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanr1' ≡ 'scanr1Of' 'traverse'@
--
-- @
-- 'scanr1Of' :: 'Iso' s t a a       -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Lens' s t a a      -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Traversal' s t a a -> (a -> a -> a) -> s -> t
-- @
scanr1Of :: LensLike (Backwards (Lazy.State (Maybe a))) s t a a -> (a -> a -> a) -> s -> t
scanr1Of l f = snd . mapAccumROf l step Nothing where
  step Nothing a  = (Just a, a)
  step (Just s) a = (Just r, r) where r = f a s
{-# INLINE scanr1Of #-}

-- | This permits the use of 'scanl1' over an arbitrary 'Traversal' or 'Lens'.
--
-- @'scanl1' ≡ 'scanl1Of' 'traverse'@
--
-- @
-- 'scanr1Of' :: 'Iso' s t a a       -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Lens' s t a a      -> (a -> a -> a) -> s -> t
-- 'scanr1Of' :: 'Traversal' s t a a -> (a -> a -> a) -> s -> t
-- @
scanl1Of :: LensLike (Lazy.State (Maybe a)) s t a a -> (a -> a -> a) -> s -> t
scanl1Of l f = snd . mapAccumLOf l step Nothing where
  step Nothing a  = (Just a, a)
  step (Just s) a = (Just r, r) where r = f s a
{-# INLINE scanl1Of #-}

-- | This 'Traversal' allows you to 'traverse' the individual stores in a 'Bazaar'.
loci :: Traversal (Bazaar a c s) (Bazaar b c s) a b
loci f w = traverse f (ins w) <&> \xs -> Bazaar $ \g -> traverse g xs <&> unsafeOuts w
{-# INLINE loci #-}

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
-- 'partsOf' :: 'Iso'' s a       -> 'Lens'' s [a]
-- 'partsOf' :: 'Lens'' s a      -> 'Lens'' s [a]
-- 'partsOf' :: 'Traversal'' s a -> 'Lens'' s [a]
-- 'partsOf' :: 'Fold' s a       -> 'Getter' s [a]
-- 'partsOf' :: 'Getter' s a     -> 'Getter' s [a]
-- @
partsOf :: Functor f => Traversing (->) f s t a a -> LensLike f s t [a] [a]
partsOf l f s = outsT b <$> f (insT b) where b = l sellT s
{-# INLINE partsOf #-}

-- | A type-restricted version of 'partsOf' that can only be used with a 'Traversal'.
partsOf' :: ATraversal s t a a -> Lens s t [a] [a]
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
-- 'unsafePartsOf' :: 'Iso' s t a b       -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Lens' s t a b      -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Traversal' s t a b -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Fold' s a          -> 'Getter' s [a]
-- 'unsafePartsOf' :: 'Getter' s a        -> 'Getter' s [a]
-- @
unsafePartsOf :: Functor f => Traversing (->) f s t a b -> LensLike f s t [a] [b]
unsafePartsOf l f s = unsafeOutsT b <$> f (insT b) where b = l sellT s
{-# INLINE unsafePartsOf #-}

unsafePartsOf' :: ATraversal s t a b -> Lens s t [a] [b]
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
-- 'holesOf' :: 'Iso'' s a       -> s -> ['Context' a a s]
-- 'holesOf' :: 'Lens'' s a      -> s -> ['Context' a a s]
-- 'holesOf' :: 'Traversal'' s a -> s -> ['Context' a a s]
-- @
holesOf :: ATraversal s t a a -> s -> [Context a a t]
holesOf l s = f (ins b) (outs b) where
  b = l sell s
  f []     _ = []
  f (x:xs) g = Context (g . (:xs)) x : f xs (g . (x:))
{-# INLINE holesOf #-}

-- | This converts a 'Traversal' that you \"know\" will target one or more elements to a 'Lens'. It can
-- also be used to transform a non-empty 'Fold' into a 'Getter' or a non-empty 'MonadicFold' into an
-- 'Action'.
--
-- The resulting 'Lens', 'Getter', or 'Action' will be partial if the supplied traversal returns
-- no results.
--
-- @
-- 'singular' :: 'Traversal' s t a a -> 'Lens' s t a a
-- 'singular' :: 'Fold' s a          -> 'Getter' s a
-- 'singular' :: 'MonadicFold' m s a -> 'Action' m s a
-- @
singular :: Functor f => Traversing (->) f s t a a -> LensLike f s t a a
singular l f = partsOf l $ \xs -> case xs of
  (a:as) -> (:as) <$> f a
  []     -> [] <$ f (error "singular: empty traversal")
{-# INLINE singular #-}

-- | This converts a 'Traversal' that you \"know\" will target only one element to a 'Lens'. It can also be
-- used to transform a 'Fold' into a 'Getter' or a 'MonadicFold' into an 'Action'.
--
-- The resulting 'Lens', 'Getter', or 'Action' will be partial if the Traversal targets nothing
-- or more than one element.
--
-- @
-- 'unsafeSingular' :: 'Traversal' s t a b -> 'Lens' s t a b
-- 'unsafeSingular' :: 'Fold' s a          -> 'Getter' s a
-- 'unsafeSingular' :: 'MonadicFold' m s a -> 'Action' m s a
-- @
unsafeSingular :: Functor f => Traversing (->) f s t a b -> LensLike f s t a b
unsafeSingular l f = unsafePartsOf l $ \xs -> case xs of
  [a] -> return <$> f a
  []  -> error "unsafeSingular: empty traversal"
  _   -> error "unsafeSingular: traversing multiple results"
{-# INLINE unsafeSingular #-}

------------------------------------------------------------------------------
-- Internal functions used by 'partsOf', 'holesOf', etc.
------------------------------------------------------------------------------
ins :: Bazaar a b t -> [a]
ins = toListOf bazaar
{-# INLINE ins #-}

outs :: Bazaar' a t -> [a] -> t
#if MIN_VERSION_mtl(2,1,1)
outs = evalState . bazaar (State.state . unconsWithDefault)
#else
outs = evalState . bazaar (\oldVal -> do (r,s) <- State.gets (unconsWithDefault oldVal); State.put s; return r)
#endif
{-# INLINE outs #-}


unsafeOuts :: Bazaar a b t -> [b] -> t
#if MIN_VERSION_mtl(2,1,1)
unsafeOuts = evalState . bazaar (\_ -> State.state (unconsWithDefault fakeVal))
#else
unsafeOuts = evalState . bazaar (\_-> do (r,s) <- State.gets (unconsWithDefault fakeVal); State.put s; return r)
#endif
  where fakeVal = error "unsafePartsOf': not enough elements were supplied"
{-# INLINE unsafeOuts #-}

insT :: BazaarT (->) f a b t -> [a]
insT = toListOf bazaarT
{-# INLINE insT #-}

outsT :: BazaarT' (->) f a t -> [a] -> t
#if MIN_VERSION_mtl(2,1,1)
outsT = evalState . bazaarT (State.state . unconsWithDefault)
#else
outsT = evalState . bazaarT (\oldVal -> do (r,s) <- State.gets (unconsWithDefault oldVal); State.put s; return r)
#endif
{-# INLINE outsT #-}

unsafeOutsT :: BazaarT (->) f a b t -> [b] -> t
#if MIN_VERSION_mtl(2,1,1)
unsafeOutsT = evalState . bazaarT (\_ -> State.state (unconsWithDefault fakeVal))
#else
unsafeOutsT = evalState . bazaarT (\_-> do (r,s) <- State.gets (unconsWithDefault fakeVal); State.put s; return r)
#endif
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
--
-- >>> over both length ("hello","world")
-- (5,5)
--
-- >>> ("hello","world")^.both
-- "helloworld"
both :: Traversal (a,a) (b,b) a b
both f ~(a,a') = (,) <$> f a <*> f a'
{-# INLINE both #-}

-- | Apply a different 'Traversal' or 'Fold' to each side of a tuple.
--
-- >>> ("hello",["world","!!!"])^..beside id traverse
-- ["hello","world","!!!"]
beside :: Applicative f => LensLike f s t a b -> LensLike f s' t' a b -> LensLike f (s,s') (t,t') a b
beside l r f ~(s,s') = (,) <$> l f s <*> r f s'
{-# INLINE beside #-}

-- | Visit the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
--
-- >>> [("hello","world"),("!!!","!!!")]^.. taking 2 (traverse.both)
-- ["hello","world"]
--
-- >>> [1..] ^.. taking 3 traverse
-- [1,2,3]
--
-- >>> over (taking 5 traverse) succ "hello world"
-- "ifmmp world"
taking :: Applicative f => Int -> Traversing' (->) f s a -> LensLike' f s a
taking n l f s = outsT b <$> traverse f (take n $ insT b) where b = l sellT s
{-# INLINE taking #-}

-- | Visit all but the first /n/ targets of a 'Traversal', 'Fold', 'Getter' or 'Lens'.
--
-- >>> ("hello","world") ^? dropping 1 both
-- Just "world"
--
-- Dropping works on infinite traversals as well:
--
-- >>> [1..]^? dropping 1 folded
-- Just 2
dropping :: Applicative f => Int -> LensLike' (Indexing f) s a -> LensLike' f s a
dropping n l f s = case runIndexing (l (\a -> Indexing $ \i -> i `seq` (if i >= n then f a else pure a, i + 1)) s) 0 of
  (r, _) -> r
{-# INLINE dropping #-}

------------------------------------------------------------------------------
-- Cloning Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' is completely characterized by its behavior on a 'Bazaar'.
--
-- Cloning a 'Traversal' is one way to make sure you aren't given
-- something weaker, such as a 'Fold' and can be
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
cloneTraversal :: ATraversal s t a b -> Traversal s t a b
cloneTraversal l f = bazaar f . l sell
{-# INLINE cloneTraversal #-}

------------------------------------------------------------------------------
-- Indexed Traversals
------------------------------------------------------------------------------

-- | Traversal with an index.
--
-- /NB:/ When you don't need access to the index then you can just apply your 'IndexedTraversal'
-- directly as a function!
--
-- @
-- 'itraverseOf' ≡ 'Control.Lens.Indexed.withIndex'
-- 'Control.Lens.Traversal.traverseOf' l = 'itraverseOf' l '.' 'const' = 'id'
-- @
--
-- @
-- 'itraverseOf' :: 'IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- 'itraverseOf' :: 'IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
itraverseOf :: (Indexed i a (f b) -> s -> f t) -> (i -> a -> f b) -> s -> f t
itraverseOf l = l . Indexed
{-# INLINE itraverseOf #-}

-- |
-- Traverse with an index (and the arguments flipped)
--
-- @
-- 'Control.Lens.Traversal.forOf' l a ≡ 'iforOf' l a '.' 'const'
-- 'iforOf' ≡ 'flip' . 'itraverseOf'
-- @
--
-- @
-- 'iforOf' :: 'IndexedLens' i s t a b      -> s -> (i -> a -> f b) -> f t
-- 'iforOf' :: 'IndexedTraversal' i s t a b -> s -> (i -> a -> f b) -> f t
-- @
iforOf :: (Indexed i a (f b) -> s -> f t) -> s -> (i -> a -> f b) -> f t
iforOf l s f = l (Indexed f) s
{-# INLINE iforOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position.
--
-- When you don't need access to the index 'mapMOf' is more liberal in what it can accept.
--
-- @'Control.Lens.Traversal.mapMOf' l ≡ 'imapMOf' l '.' 'const'@
--
-- @
-- 'imapMOf' :: 'Monad' m => 'IndexedLens'      i s t a b -> (i -> a -> m b) -> s -> m t
-- 'imapMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> (i -> a -> m b) -> s -> m t
-- @
imapMOf :: (Indexed i a (WrappedMonad m b) -> s -> WrappedMonad m t) -> (i -> a -> m b) -> s -> m t
imapMOf l f = unwrapMonad . l (Indexed $ \i -> WrapMonad . f i)
{-# INLINE imapMOf #-}

-- | Map each element of a structure targeted by a lens to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'Control.Lens.Traversal.forMOf' l a ≡ 'iforMOf' l a '.' 'const'
-- 'iforMOf' ≡ 'flip' '.' 'imapMOf'
-- @
--
-- @
-- 'iforMOf' :: 'Monad' m => 'IndexedLens' i s t a b      -> s -> (i -> a -> m b) -> m t
-- 'iforMOf' :: 'Monad' m => 'IndexedTraversal' i s t a b -> s -> (i -> a -> m b) -> m t
-- @
iforMOf :: (Indexed i a (WrappedMonad m b) -> s -> WrappedMonad m t) -> s -> (i -> a -> m b) -> m t
iforMOf = flip . imapMOf
{-# INLINE iforMOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @'Control.Lens.Traversal.mapAccumROf' l ≡ 'imapAccumROf' l '.' 'const'@
--
-- @
-- 'imapAccumROf' :: 'IndexedLens' i s t a b      -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- 'imapAccumROf' :: 'IndexedTraversal' i s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- @
imapAccumROf :: (Indexed i a (Lazy.State s b) -> s -> Lazy.State s t) -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
imapAccumROf l f s0 a = swap (Lazy.runState (l (Indexed $ \i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumROf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IndexedTraversal' with access to the index.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @'Control.Lens.Traversal.mapAccumLOf' l ≡ 'imapAccumLOf' l '.' 'const'@
--
-- @
-- 'imapAccumLOf' :: 'IndexedLens' i s t a b      -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- 'imapAccumLOf' :: 'IndexedTraversal' i s t a b -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
-- @
imapAccumLOf :: (Indexed i a (Backwards (Lazy.State s) b) -> s -> Backwards (Lazy.State s) t) -> (i -> s -> a -> (s, b)) -> s -> s -> (s, t)
imapAccumLOf l f s0 a = swap (Lazy.runState (forwards (l (Indexed $ \i c -> Backwards (Lazy.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumLOf #-}

------------------------------------------------------------------------------
-- Common Indexed Traversals
------------------------------------------------------------------------------

-- | Access the element of an 'IndexedTraversal' where the index matches a predicate.
--
-- >>> over (iwhereOf traversed (>0)) reverse ["He","was","stressed","o_O"]
-- ["He","saw","desserts","O_o"]
--
-- @
-- 'iwhereOf' :: 'IndexedFold' i s a       -> (i -> 'Bool') -> 'IndexedFold' i s a
-- 'iwhereOf' :: 'IndexedGetter' i s a     -> (i -> 'Bool') -> 'IndexedFold' i s a
-- 'iwhereOf' :: 'IndexedLens'' i s a      -> (i -> 'Bool') -> 'IndexedTraversal'' i s a
-- 'iwhereOf' :: 'IndexedTraversal'' i s a -> (i -> 'Bool') -> 'IndexedTraversal'' i s a
-- 'iwhereOf' :: 'IndexedSetter'' i s a    -> (i -> 'Bool') -> 'IndexedSetter'' i s a
-- @
iwhereOf :: (Indexable i p, Applicative f) => IndexedLensLike (Indexed i) f s t a a -> (i -> Bool) -> IndexedLensLike p f s t a a
iwhereOf l p f = l . Indexed $ \i a -> if p i then indexed f i a else pure a
{-# INLINE iwhereOf #-}

-- | Traverse any 'Traversable' container. This is an 'IndexedTraversal' that is indexed by ordinal position.
traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b
traversed = indexing traverse
{-# INLINE traversed #-}

-- | Traverse any 'Traversable' container. This is an 'IndexedTraversal' that is indexed by ordinal position.
traversed64 :: Traversable f => IndexedTraversal Int64 (f a) (f b) a b
traversed64 = indexing64 traverse
{-# INLINE traversed64 #-}

-- | This provides a 'Traversal' that checks a predicate on a key before
-- allowing you to traverse into a value.
value :: (k -> Bool) -> IndexedTraversal' k (k, v) v
value p f kv@(k,v)
  | p k       = (,) k <$> indexed f k v
  | otherwise = pure kv
{-# INLINE value #-}

-- | This is the trivial empty traversal.
--
-- @'ignored' :: 'IndexedTraversal' i s s a b@
--
-- @'ignored' ≡ 'const' 'pure'@
ignored :: Applicative f => kafb -> s -> f s
ignored _ = pure
{-# INLINE ignored #-}

-- | Allows 'IndexedTraversal' the value at the smallest index.
class Ord k => TraverseMin k m | m -> k where
  -- | 'IndexedTraversal' of the element with the smallest index.
  traverseMin :: IndexedTraversal' k (m v) v

instance TraverseMin Int IntMap where
  traverseMin f m = case IntMap.minViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMin (const (Just v)) m
#else
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMin (const v) m
#endif
    Nothing     -> pure m
  {-# INLINE traverseMin #-}

instance Ord k => TraverseMin k (Map k) where
  traverseMin f m = case Map.minViewWithKey m of
    Just ((k, a), _) -> indexed f k a <&> \v -> Map.updateMin (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMin #-}

-- | Allows 'IndexedTraversal' of the value at the largest index.
class Ord k => TraverseMax k m | m -> k where
  -- | 'IndexedTraversal' of the element at the largest index.
  traverseMax :: IndexedTraversal' k (m v) v

instance TraverseMax Int IntMap where
  traverseMax f m = case IntMap.maxViewWithKey m of
#if MIN_VERSION_containers(0,5,0)
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMax (const (Just v)) m
#else
    Just ((k,a), _) -> indexed f k a <&> \v -> IntMap.updateMax (const v) m
#endif
    Nothing     -> pure m
  {-# INLINE traverseMax #-}

instance Ord k => TraverseMax k (Map k) where
  traverseMax f m = case Map.maxViewWithKey m of
    Just ((k, a), _) -> indexed f k a <&> \v -> Map.updateMax (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMax #-}

-- | Traverse the /nth/ element 'elementOf' a 'Traversal', 'Lens' or
-- 'Iso' if it exists.
--
-- >>> [[1],[3,4]] & elementOf (traverse.traverse) 1 .~ 5
-- [[1],[5,4]]
--
-- >>> [[1],[3,4]] ^? elementOf (folded.folded) 1
-- Just 3
--
-- >>> [0..] ^?! elementOf folded 5
-- 5
--
-- >>> take 10 $ elementOf traverse 3 .~ 16 $ [0..]
-- [0,1,2,16,4,5,6,7,8,9]
--
-- @
-- 'elementOf' :: 'Traversal'' s a -> Int -> 'IndexedTraversal'' 'Int' s a
-- 'elementOf' :: 'Fold' s a       -> Int -> 'IndexedFold' 'Int' s a
-- @
elementOf :: (Applicative f, Indexable Int p)
          => LensLike (Indexing f) s t a a
          -> Int
          -> IndexedLensLike p f s t a a
elementOf l p = elementsOf l (p ==)
{-# INLINE elementOf #-}

-- | Traverse the /nth/ element of a 'Traversable' container.
--
-- @'element' ≡ 'elementOf' 'traverse'@
element :: Traversable t => Int -> IndexedTraversal' Int (t a) a
element = elementOf traverse
{-# INLINE element #-}

-- | Traverse (or fold) selected elements of a 'Traversal' (or 'Fold') where their ordinal positions match a predicate.
--
-- @
-- 'elementsOf' :: 'Traversal'' s a -> ('Int' -> 'Bool') -> 'IndexedTraversal'' 'Int' s a
-- 'elementsOf' :: 'Fold' s a       -> ('Int' -> 'Bool') -> 'IndexedFold' 'Int' s a
-- @
elementsOf :: (Applicative f, Indexable Int p)
           => LensLike (Indexing f) s t a a
           -> (Int -> Bool)
           -> IndexedLensLike p f s t a a
elementsOf l p iafb s =
  case runIndexing (l (\a -> Indexing (\i -> i `seq` (if p i then indexed iafb i a else pure a, i + 1))) s) 0 of
    (r, _) -> r
{-# INLINE elementsOf #-}

-- | Traverse elements of a 'Traversable' container where their ordinal positions matches a predicate.
--
-- @'elements' ≡ 'elementsOf' 'traverse'@
elements :: Traversable t => (Int -> Bool) -> IndexedTraversal' Int (t a) a
elements = elementsOf traverse
{-# INLINE elements #-}
