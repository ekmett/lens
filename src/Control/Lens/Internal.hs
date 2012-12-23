{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- These are some of the explicit Functor instances that leak into the
-- type signatures of Control.Lens. You shouldn't need to import this
-- module directly, unless you are coming up with a whole new kind of
-- \"Family\" and need to add instances.
--
----------------------------------------------------------------------------
module Control.Lens.Internal
  (
  -- * Internal Classes
  -- ** Getters
    Gettable(..)
  , noEffect
  -- ** Actions
  , Effective(..)
  -- ** Setters
  , Settable(..)
  -- * Prisms
  , Prismatic(..)
  -- ** Indexable
  , Indexable(..)
  -- ** Strict Composition
  , NewtypeComposition(..)
  -- * Internal Types
  , May(..)
  , Effect(..)
  , EffectRWS(..)
  , Err(..)
  , Traversed(..)
  , Sequenced(..)
  , Focusing(..)
  , FocusingWith(..)
  , FocusingPlus(..)
  , FocusingOn(..)
  , FocusingMay(..)
  , FocusingErr(..)
  , Folding(..)
  , Max(..), getMax
  , Min(..), getMin
  , Indexing(..), indexing
  , Indexing64(..), indexing64
  -- * Common Types
  , Accessor(..)
  , Mutator(..)
  , Context(..)
  , Bazaar(..), bazaar, duplicateBazaar, sell
  , BazaarT(..), bazaarT, duplicateBazaarT, sellT
  , Review(..)
  , Exchange(..)
  , Market(..)
  , Indexed(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Int
import Data.Monoid
import Data.Profunctor
#ifndef SAFE
import Unsafe.Coerce
#endif

{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}
{-# ANN module "HLint: ignore Use const" #-}

#ifndef SAFE
#define UNSAFELY(x) unsafeCoerce
#else
#define UNSAFELY(f) (\g -> g `seq` \x -> (f) (g x))
#endif

infixr 9 #.
infixl 8 .#

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | Generalizing 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages
--
-- A 'Gettable' 'Functor' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- To ensure this, an instance of 'Gettable' is required to satisfy:
--
-- @'id' = 'fmap' f = 'coerce'@
--
-- Which is equivalent to making a @'Gettable' f@ an \"anyvariant\" functor.

class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m
  {-# INLINE coerce #-}

instance Gettable (Accessor r) where
  coerce (Accessor m) = Accessor m
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards
  {-# INLINE coerce #-}

instance Gettable (Effect m r) where
  coerce (Effect m) = Effect m
  {-# INLINE coerce #-}

instance Gettable (EffectRWS w st m s) where
  coerce (EffectRWS m) = EffectRWS m
  {-# INLINE coerce #-}

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Indexing f) where
  coerce (Indexing m) = Indexing $ \i -> case m i of
    (ff, j) -> (coerce ff, j)
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Indexing64 f) where
  coerce (Indexing64 m) = Indexing64 $ \i -> case m i of
    (ff, j) -> (coerce ff, j)
  {-# INLINE coerce #-}

instance Gettable g => Gettable (BazaarT a b g) where
  coerce = (<$) (error "coerced BazaarT")
  {-# INLINE coerce #-}

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Applicative f, Gettable f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}

-------------------------------------------------------------------------------
-- Programming with Effects
-------------------------------------------------------------------------------

-- | An 'Effective' 'Functor' ignores its argument and is isomorphic to a 'Monad' wrapped around a value.
--
-- That said, the 'Monad' is possibly rather unrelated to any 'Applicative' structure.
class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: m r -> f a
  ineffective :: f a -> m r

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = Backwards . effective . liftM getDual
  {-# INLINE effective #-}
  ineffective = liftM Dual . ineffective . forwards
  {-# INLINE ineffective #-}

instance Monad m => Effective m r (Effect m r) where
  effective = Effect
  {-# INLINE effective #-}
  ineffective = getEffect
  {-# INLINE ineffective #-}

-- Effective EffectRWS ?

instance Effective Identity r (Accessor r) where
  effective = Accessor . runIdentity
  {-# INLINE effective #-}
  ineffective = Identity . runAccessor
  {-# INLINE ineffective #-}

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class Applicative f => Settable f where
  untainted :: f a -> a

  untaintedDot :: (a -> f b) -> a -> b
  untaintedDot g = g `seq` \x -> untainted (g x)
  {-# INLINE untaintedDot #-}

  taintedDot :: (a -> b) -> a -> f b
  taintedDot g = g `seq` \x -> pure (g x)
  {-# INLINE taintedDot #-}

-- | so you can pass our a 'Control.Lens.Setter.Setter' into combinators from other lens libraries
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = UNSAFELY(runIdentity)
  {-# INLINE untaintedDot #-}
  taintedDot = UNSAFELY(Identity)
  {-# INLINE taintedDot #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untaintedDot forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untaintedDot (untaintedDot getCompose)
  {-# INLINE untainted #-}

instance Settable Mutator where
  untainted = runMutator
  {-# INLINE untainted #-}
  untaintedDot = UNSAFELY(runMutator)
  {-# INLINE untaintedDot #-}
  taintedDot = UNSAFELY(Mutator)
  {-# INLINE taintedDot #-}

-----------------------------------------------------------------------------
-- Prism Internals
-----------------------------------------------------------------------------

class Profunctor p => Prismatic p where
  prismatic :: p a b -> p (Either b a) b

instance Prismatic (->) where
  prismatic = either id
  {-# INLINE prismatic #-}

-----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Profunctor p => Indexable i p where
  -- | Build a function from an 'Indexed' function
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed = const
  {-# INLINE indexed #-}

-----------------------------------------------------------------------------
-- Strict Composition
-----------------------------------------------------------------------------

-- $strict
-- These combinators are used to reduce eta-expansion in the resulting code
-- which could otherwise cause both a constant and asymptotic slowdown to
-- code execution.
--
-- Many micro-benchmarks are improved up to 50%, and larger benchmarks can
-- win asymptotically.

class NewtypeComposition a b where
  ( #. ) :: (a -> b) -> (c -> a) -> c -> b
  ( #. ) = \f -> f `seq` \g -> g `seq` \x -> f (g x)
  {-# INLINE ( #. ) #-}

  ( .# ) :: (b -> c) -> (a -> b) -> a -> c
  ( .# ) = \f -> f `seq` \g -> g `seq` \x -> f (g x)
  {-# INLINE ( .# ) #-}

#ifndef SAFE
#define COMPOSE(a, b, f, g) \
  instance NewtypeComposition (a) (b) where { \
    ( #. ) = \_ -> unsafeCoerce; \
    {-# INLINE ( #. ) #-}; \
    ( .# ) = \h -> \_ -> unsafeCoerce h; \
    {-# INLINE ( .# ) #-}; \
  }; \
  instance NewtypeComposition (b) (a) where { \
    ( #. ) = \_ -> unsafeCoerce; \
    {-# INLINE ( #. ) #-}; \
    ( .# ) = \h -> \_ -> unsafeCoerce h; \
    {-# INLINE ( .# ) #-}; \
  }
#else
#define COMPOSE(a, b, f, g) \
  instance NewtypeComposition (a) (b) where { \
    ( #. ) = \_ -> \h -> h `seq` \x -> (g) (h x); \
    {-# INLINE ( #. ) #-}; \
    ( .# ) = \h -> h `seq` \_ -> \x -> h ((g) x); \
    {-# INLINE ( .# ) #-}; \
  }; \
  instance NewtypeComposition (b) (a) where { \
    ( #. ) = \_ -> \h -> h `seq` \x -> (f) (h x); \
    {-# INLINE ( #. ) #-}; \
    ( .# ) = \h -> h `seq` \_ -> \x -> h ((f) x); \
    {-# INLINE ( .# ) #-}; \
  }
#endif

COMPOSE(Const r a, r, Const, getConst)
COMPOSE(ZipList a, [a], ZipList, getZipList)
COMPOSE(WrappedMonad m a, m a, WrapMonad, unwrapMonad)
COMPOSE(Last a, Maybe a, Last, getLast)
COMPOSE(First a, Maybe a, First, getFirst)
COMPOSE(Product a, a, Product, getProduct)
COMPOSE(Sum a, a, Sum, getSum)
COMPOSE(Any, Bool, Any, getAny)
COMPOSE(All, Bool, All, getAll)
COMPOSE(Dual a, a, Dual, getDual)
COMPOSE(Endo a, a -> a, Endo, appEndo)
COMPOSE(May a, Maybe a, May, getMay)
COMPOSE(Folding f a, f a, Folding, getFolding)
COMPOSE(Effect m r a, m r, Effect, getEffect)
COMPOSE(EffectRWS w st m s a, st -> m (s, st, w), EffectRWS, getEffectRWS)
COMPOSE(Accessor r a, r, Accessor, runAccessor)
COMPOSE(Err e a, Either e a, Err, getErr)
COMPOSE(Traversed f, f (), Traversed, getTraversed)
COMPOSE(Sequenced f, f (), Sequenced, getSequenced)
COMPOSE(Focusing m s a, m (s, a), Focusing, unfocusing)
COMPOSE(FocusingWith w m s a, m (s, a, w), FocusingWith, unfocusingWith)
COMPOSE(FocusingPlus w k s a, k (s, w) a, FocusingPlus, unfocusingPlus)
COMPOSE(FocusingOn f k s a, k (f s) a, FocusingOn, unfocusingOn)
COMPOSE(FocusingMay k s a, k (May s) a, FocusingMay, unfocusingMay)
COMPOSE(FocusingErr e k s a, k (Err e s) a, FocusingErr, unfocusingErr)
COMPOSE(Mutator a, a, Mutator, runMutator)
COMPOSE(Backwards f a, f a, Backwards, forwards)
COMPOSE(Compose f g a, f (g a), Compose, getCompose)
COMPOSE(Cokleisli f a b, f a -> b, Cokleisli, runCokleisli)
COMPOSE(Indexed i s t, i -> s -> t, Indexed, runIndexed)

------------------------------------------------------------------------------
-- Internal Types
------------------------------------------------------------------------------

-- | A 'Monoid' for a 'Gettable' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Gettable f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}

-----------------------------------------------------------------------------
-- Functors
-----------------------------------------------------------------------------

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.State.StateT'
newtype Focusing m s a = Focusing { unfocusing :: m (s, a) }

instance Monad m => Functor (Focusing m s) where
  fmap f (Focusing m) = Focusing $ do
     (s, a) <- m
     return (s, f a)

instance (Monad m, Monoid s) => Applicative (Focusing m s) where
  pure a = Focusing (return (mempty, a))
  Focusing mf <*> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (mappend s s', f a)

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.RWS.RWST'
newtype FocusingWith w m s a = FocusingWith { unfocusingWith :: m (s, a, w) }

instance Monad m => Functor (FocusingWith w m s) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (s, a, w) <- m
     return (s, f a, w)

instance (Monad m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure a = FocusingWith (return (mempty, a, mempty))
  FocusingWith mf <*> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (mappend s s', f a, mappend w w')

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)

instance (Monoid w, Applicative (k (s, w))) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'
newtype FocusingOn f k s a = FocusingOn { unfocusingOn :: k (f s) a }

instance Functor (k (f s)) => Functor (FocusingOn f k s) where
  fmap f (FocusingOn as) = FocusingOn (fmap f as)

instance Applicative (k (f s)) => Applicative (FocusingOn f k s) where
  pure = FocusingOn . pure
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)

-- | Make a monoid out of 'Maybe' for error handling
newtype May a = May { getMay :: Maybe a }

instance Monoid a => Monoid (May a) where
  mempty = May (Just mempty)
  May Nothing `mappend` _ = May Nothing
  _ `mappend` May Nothing = May Nothing
  May (Just a) `mappend` May (Just b) = May (Just (mappend a b))

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingMay k s a = FocusingMay { unfocusingMay :: k (May s) a }

instance Functor (k (May s)) => Functor (FocusingMay k s) where
  fmap f (FocusingMay as) = FocusingMay (fmap f as)

instance Applicative (k (May s)) => Applicative (FocusingMay k s) where
  pure = FocusingMay . pure
  FocusingMay kf <*> FocusingMay ka = FocusingMay (kf <*> ka)

-- | Make a monoid out of 'Either' for error handling
newtype Err e a = Err { getErr :: Either e a }

instance Monoid a => Monoid (Err e a) where
  mempty = Err (Right mempty)
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingErr e k s a = FocusingErr { unfocusingErr :: k (Err e s) a }

instance Functor (k (Err e s)) => Functor (FocusingErr e k s) where
  fmap f (FocusingErr as) = FocusingErr (fmap f as)

instance Applicative (k (Err e s)) => Applicative (FocusingErr e k s) where
  pure = FocusingErr . pure
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)

-- | Applicative composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'
newtype Indexing f a = Indexing { runIndexing :: Int -> (f a, Int) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (x, j) -> (fmap f x, j)

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing (\i -> (pure x, i))
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (ff, j) -> case ma j of
       ~(fa, k) -> (ff <*> fa, k)

-- | Transform a 'Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- @
-- 'indexing' :: 'Control.Lens.Traversal.Traversal' s t a b -> 'Control.Lens.Traversal.IndexedTraversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Prism.Prism' s t a b     -> 'Control.Lens.Traversal.IndexedTraversal' 'Int' s t a b
-- 'indexing' :: 'Lens' s t a b      -> 'IndexedLens' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Iso.Iso' s t a b       -> 'IndexedLens' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Fold.Fold' s t          -> 'Control.Lens.Fold.IndexedFold' 'Int' s t
-- 'indexing' :: 'Control.Lens.Getter.Getter' s t        -> 'Control.Lens.Getter.IndexedGetter' 'Int' s t a b
-- @
indexing :: Indexable Int p => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> s -> f t
indexing l iafb s = case runIndexing (l (\a -> Indexing (\i -> i `seq` (indexed iafb i a, i + 1))) s) 0 of
  (r, _) -> r
{-# INLINE indexing #-}

-- | Applicative composition of @'Control.Monad.Trans.State.Lazy.State' 'Int64'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed64'
newtype Indexing64 f a = Indexing64 { runIndexing64 :: Int64 -> (f a, Int64) }

instance Functor f => Functor (Indexing64 f) where
  fmap f (Indexing64 m) = Indexing64 $ \i -> case m i of
    (x, j) -> (fmap f x, j)

instance Applicative f => Applicative (Indexing64 f) where
  pure x = Indexing64 (\i -> (pure x, i))
  Indexing64 mf <*> Indexing64 ma = Indexing64 $ \i -> case mf i of
    (ff, j) -> case ma j of
       ~(fa, k) -> (ff <*> fa, k)

-- | Transform a 'Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- This combinator is like 'indexing' except that it handles large 'Traversal's and 'Fold's gracefully.
--
-- @
-- 'indexing64' :: 'Control.Lens.Traversal.Traversal' s t a b -> 'Control.Lens.Traversal.IndexedTraversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Prism.Prism' s t a b     -> 'Control.Lens.Traversal.IndexedTraversal' 'Int64' s t a b
-- 'indexing64' :: 'Lens' s t a b      -> 'IndexedLens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Iso.Iso' s t a b       -> 'IndexedLens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Fold.Fold' s t          -> 'Control.Lens.Fold.IndexedFold' 'Int64' s t
-- 'indexing64' :: 'Control.Lens.Getter.Getter' s t        -> 'Control.Lens.Getter.IndexedGetter' 'Int64' s t a b
-- @
indexing64 :: Indexable Int64 p => ((a -> Indexing64 f b) -> s -> Indexing64 f t) -> p a (f b) -> s -> f t
indexing64 l iafb s = case runIndexing64 (l (\a -> Indexing64 (\i -> i `seq` (indexed iafb i a, i + 1))) s) 0 of
  (r, _) -> r
{-# INLINE indexing64 #-}

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
newtype Traversed f = Traversed { getTraversed :: f () }

instance Applicative f => Monoid (Traversed f) where
  mempty = Traversed (pure ())
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
newtype Sequenced m = Sequenced { getSequenced :: m () }

instance Monad m => Monoid (Sequenced m) where
  mempty = Sequenced (return ())
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)

-- | Used for 'Control.Lens.Fold.minimumOf'
data Min a = NoMin | Min a

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  mappend NoMin m = m
  mappend m NoMin = m
  mappend (Min a) (Min b) = Min (min a b)

-- | Obtain the minimum.
getMin :: Min a -> Maybe a
getMin NoMin   = Nothing
getMin (Min a) = Just a

-- | Used for 'Control.Lens.Fold.maximumOf'
data Max a = NoMax | Max a

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  mappend NoMax m = m
  mappend m NoMax = m
  mappend (Max a) (Max b) = Max (max a b)

-- | Obtain the maximum
getMax :: Max a -> Maybe a
getMax NoMax   = Nothing
getMax (Max a) = Just a

-- | The indexed store can be used to characterize a 'Control.Lens.Lens.Lens'
-- and is used by 'Control.Lens.Lens.clone'
--
-- @'Context' a b t@ is isomorphic to
-- @newtype Context a b t = Context { runContext :: forall f. Functor f => (a -> f b) -> f t }@,
-- and to @exists s. (s, 'Control.Lens.Lens.Lens' s t a b)@.
--
-- A 'Context' is like a 'Control.Lens.Lens.Lens' that has already been applied to a some structure.
data Context a b t = Context (b -> t) a

instance Functor (Context a b) where
  fmap f (Context g t) = Context (f . g) t

instance (a ~ b) => Comonad (Context a b) where
  extract   (Context f a) = f a
  duplicate (Context f a) = Context (Context f) a
  extend g  (Context f a) = Context (g . Context f) a

instance (a ~ b) => ComonadStore a (Context a b) where
  pos (Context _ a) = a
  peek b (Context g _) = g b
  peeks f (Context g a) = g (f a)
  seek a (Context g _) = Context g a
  seeks f (Context g a) = Context g (f a)
  experiment f (Context g a) = g <$> f a

-- | This is used to characterize a 'Control.Lens.Traversal.Traversal'.
--
-- a.k.a. indexed Cartesian store comonad, indexed Kleene store comonad, or an indexed 'FunList'.
--
-- <http://twanvl.nl/blog/haskell/non-regular1>
--
-- @'Bazaar' a b t@ is isomorphic to @data Bazaar a b t = Buy t | Trade (Bazaar a b (b -> t)) a@,
-- and to @exists s. (s, 'Control.Lens.Traversal.Traversal' s t a b)@.
--
-- A 'Bazaar' is like a 'Control.Lens.Traversal.Traversal' that has already been applied to some structure.
--
-- Where a @'Context' a b t@ holds an @a@ and a function from @b@ to
-- @t@, a @'Bazaar' a b t@ holds N @a@s and a function from N
-- @b@s to @t@.
--
-- Mnemonically, a 'Bazaar' holds many stores and you can easily add more.
--
-- This is a final encoding of 'Bazaar'.
newtype Bazaar a b t = Bazaar { runBazaar :: forall f. Applicative f => (a -> f b) -> f t }

instance Functor (Bazaar a b) where
  fmap f (Bazaar k) = Bazaar (fmap f . k)

instance Applicative (Bazaar a b) where
  pure a = Bazaar (\_ -> pure a)
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar (\k -> mf k <*> ma k)
  {-# INLINE (<*>) #-}

instance (a ~ b) => Comonad (Bazaar a b) where
  extract (Bazaar m) = runIdentity (m Identity)
  {-# INLINE extract #-}
  duplicate = duplicateBazaar
  {-# INLINE duplicate #-}

-- | Given an action to run for each matched pair, traverse a bazaar.
--
-- @'bazaar' :: 'Control.Lens.Traversal.Traversal' ('Bazaar' a b t) t a b@
bazaar :: Applicative f => (a -> f b) -> Bazaar a b t -> f t
bazaar afb (Bazaar m) = m afb
{-# INLINE bazaar #-}

-- | 'Bazaar' is an indexed 'Comonad'.
duplicateBazaar :: Bazaar a c t -> Bazaar a b (Bazaar b c t)
duplicateBazaar (Bazaar m) = getCompose (m (Compose . fmap sell . sell))
{-# INLINE duplicateBazaar #-}

-- | A trivial 'Bazaar'.
sell :: a -> Bazaar a b b
sell i = Bazaar (\k -> k i)
{-# INLINE sell #-}

instance a ~ b => ComonadApply (Bazaar a b) where
  (<@>) = (<*>)

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }

instance Functor (Effect m r) where
  fmap _ (Effect m) = Effect m

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)

-- | Wrap a monadic effect with a phantom type argument. Used when magnifying RWST.
newtype EffectRWS w st m s a = EffectRWS { getEffectRWS :: st -> m (s,st,w) }

instance Functor (EffectRWS w st m s) where
  fmap _ (EffectRWS m) = EffectRWS m

instance (Monoid s, Monoid w, Monad m) => Applicative (EffectRWS w st m s) where
  pure _ = EffectRWS $ \st -> return (mempty, st, mempty)
  EffectRWS m <*> EffectRWS n = EffectRWS $ \st -> m st >>= \ (s,t,w) -> n t >>= \ (s',u,w') -> return (mappend s s', u, mappend w w')

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

-- | Used instead of 'Const' to report
--
-- @No instance of ('Settable' 'Accessor')@
--
-- when the user attempts to misuse a 'Control.Lens.Setter.Setter' as a
-- 'Control.Lens.Getter.Getter', rather than a monolithic unification error.
newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m
  {-# INLINE fmap #-}

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  {-# INLINE pure #-}
  Accessor a <*> Accessor b = Accessor (mappend a b)
  {-# INLINE (<*>) #-}

-----------------------------------------------------------------------------
-- Mutators
-----------------------------------------------------------------------------

-- | 'Mutator' is just a renamed 'Identity' functor to give better error
-- messages when someone attempts to use a getter as a setter.
--
-- Most user code will never need to see this type.
newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator (f a)
  {-# INLINE fmap #-}

instance Applicative Mutator where
  pure = Mutator
  {-# INLINE pure #-}
  Mutator f <*> Mutator a = Mutator (f a)
  {-# INLINE (<*>) #-}

instance Monad Mutator where
  return = Mutator
  {-# INLINE return #-}
  Mutator x >>= f = f x
  {-# INLINE (>>=) #-}

-- | 'BazaarT' is like 'Bazaar', except that it provides a questionable 'Gettable' instance
-- To protect this instance it relies on the soundness of another 'Gettable' type, and usage conventions.
--
-- For example. This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there
-- must be a better way!
--
newtype BazaarT a b (g :: * -> *) t = BazaarT { runBazaarT :: forall f. Applicative f => (a -> f b) -> f t }

instance Functor (BazaarT a b g) where
  fmap f (BazaarT k) = BazaarT (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (BazaarT a b g) where
  pure a = BazaarT (\_ -> pure a)
  {-# INLINE pure #-}
  BazaarT mf <*> BazaarT ma = BazaarT (\k -> mf k <*> ma k)
  {-# INLINE (<*>) #-}

instance (a ~ b) => Comonad (BazaarT a b g) where
  extract (BazaarT m) = runIdentity (m Identity)
  {-# INLINE extract #-}
  duplicate = duplicateBazaarT
  {-# INLINE duplicate #-}

-- | Extract from a 'BazaarT'.
--
-- @'bazaarT' = 'flip' 'runBazaarT'@
bazaarT :: Applicative f => (a -> f b) -> BazaarT a b g t -> f t
bazaarT afb (BazaarT m) = m afb
{-# INLINE bazaarT #-}

-- | 'BazaarT' is an indexed 'Comonad'.
duplicateBazaarT :: BazaarT a c f t -> BazaarT a b f (BazaarT b c f t)
duplicateBazaarT (BazaarT m) = getCompose (m (Compose . fmap sellT . sellT))
{-# INLINE duplicateBazaarT #-}

-- | A trivial 'BazaarT'.
sellT :: a -> BazaarT a b f b
sellT i = BazaarT (\k -> k i)
{-# INLINE sellT #-}

------------------------------------------------------------------------------
-- Isomorphism and Prism Internals
------------------------------------------------------------------------------

data Review a b = Review { reviewed :: b }

instance Functor (Review a) where
  fmap bc (Review b) = Review (bc b)
  {-# INLINE fmap #-}

instance Profunctor Review where
  lmap _ (Review c) = Review c
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}

instance Prismatic Review where
  prismatic (Review b) = Review b
  {-# INLINE prismatic #-}

newtype Exchange a b s t = Exchange { runExchange :: (s -> a, b -> t) }

instance Functor (Exchange a b s) where
  fmap f x = case runExchange x of
    (sa, bt) -> Exchange (sa, f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  lmap f x = case runExchange x of
    (sa, bt) -> Exchange (sa . f, bt)
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}

newtype Market a b s t = Market { runMarket :: (b -> t, s -> Either t a) }

instance Functor (Market a b s) where
  fmap f x = case runMarket x of
    (bt, seta) -> Market (f . bt, either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  lmap f x = case runMarket x of
    (bt, seta) -> Market (bt, seta . f)
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}

instance Prismatic (Market a b) where
  prismatic x = case runMarket x of
    (bt, seta) -> Market (bt, either Left seta)
  {-# INLINE prismatic #-}

------------------------------------------------------------------------------
-- Indexed Internals
------------------------------------------------------------------------------

-- | A function with access to a index. This constructor may be useful when you need to store
-- an 'Indexable' in a container to avoid @ImpredicativeTypes@.
--
-- @index :: Indexed i a b -> i -> a -> b@
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Profunctor (Indexed i) where
  lmap ab ibc = Indexed (\i -> runIndexed ibc i . ab)
  {-# INLINE lmap #-}
  rmap bc iab = Indexed (\i -> bc . runIndexed iab i)
  {-# INLINE rmap #-}

-- | Using an equality witness to avoid potential overlapping instances
-- and aid dispatch.
instance i ~ j => Indexable i (Indexed j) where
  indexed = runIndexed
  {-# INLINE indexed #-}
