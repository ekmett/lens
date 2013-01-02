{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
-- This module also includes orphan instances for ((,) e) that should be
-- supplied by base.
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
  -- ** Indexable
  , SelfAdjoint(..)
  , Indexable(..)
  -- ** Measurable
  , Measurable(..)
  , Measured(..)
  -- ** Strict Composition
  , NewtypeComposition(..)
  -- ** Indexed Functors
  , IndexedFunctor(..)
  -- ** Indexed Comonads
  , IndexedComonad(..)
  -- ** Indexed Store Comonad
  , IndexedComonadStore(..)
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
  , Review(..)
  , Exchange(..)
  , Market(..), Market'
  , Identical(..)
  , Indexed(..)
  , Context(..), Context'
  , Pretext(..), Pretext'
  , PretextT(..), PretextT'
  , Bizarre(..)
  , Bazaar(..), Bazaar'
  , BazaarT(..), BazaarT'
  , Sellable(..)
  , Size(..)
  , Size64(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Arrow as Arrow
import Control.Category
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Data.Bifunctor as Bifunctor
import Data.Distributive
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Int
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Representable
import Data.Profunctor.Corepresentable
import Data.Traversable
#ifndef SAFE
import Unsafe.Coerce
#endif
import Prelude hiding ((.),id)

{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}
{-# ANN module "HLint: ignore Use const" #-}

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
  coerce = Backwards #. coerce .# forwards
  {-# INLINE coerce #-}

instance Gettable (Effect m r) where
  coerce (Effect m) = Effect m
  {-# INLINE coerce #-}

instance Gettable (EffectRWS w st m s) where
  coerce (EffectRWS m) = EffectRWS m
  {-# INLINE coerce #-}

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose #. fmap coerce .# getCompose
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Indexing f) where
  coerce (Indexing m) = Indexing $ \i -> case m i of
    (j, ff) -> (j, coerce ff)
  {-# INLINE coerce #-}

instance Gettable f => Gettable (Indexing64 f) where
  coerce (Indexing64 m) = Indexing64 $ \i -> case m i of
    (j, ff) -> (j, coerce ff)
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
  effective = Accessor #. runIdentity
  {-# INLINE effective #-}
  ineffective = Identity #. runAccessor
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
  untaintedDot = (runIdentity #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Identity #.)
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
  untaintedDot = (runMutator #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Mutator #.)
  {-# INLINE taintedDot #-}

-----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- This is a profunctor that is both corepresentable by @f@ and representable by @g@ such
-- that @f@ is left Adjoint to @g@. From this you can derive a lot of structure due
-- to the preservation of limits and colimits.
class
  ( Profunctor p, Prismatic p, Lenticular p
  , RepresentableProfunctor p, Comonad (Rep p), Traversable (Rep p)
  , CorepresentableProfunctor p, Monad (Corep p), Distributive (Corep p)
  , ArrowLoop p, ArrowApply p, ArrowChoice p
  ) => SelfAdjoint p where
  distrib :: Functor f => p a b -> p (f a) (f b)
  distrib = cotabulatePro . collect . coindexPro

instance SelfAdjoint (->) where
  distrib = fmap
  {-# INLINE distrib #-}

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class SelfAdjoint p => Indexable i p where
  -- | Build a function from an 'Indexed' function
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed = const
  {-# INLINE indexed #-}

-- | A function with access to a index. This constructor may be useful when you need to store
-- an 'Indexable' in a container to avoid @ImpredicativeTypes@.
--
-- @index :: Indexed i a b -> i -> a -> b@
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Functor (Indexed i a) where
  fmap g (Indexed f) = Indexed $ \i a -> g (f i a)
  {-# INLINE fmap #-}

instance Applicative (Indexed i a) where
  pure b = Indexed $ \_ _ -> b
  {-# INLINE pure #-}
  Indexed f <*> Indexed g = Indexed $ \i a -> f i a (g i a)
  {-# INLINE (<*>) #-}

instance Monad (Indexed i a) where
  return b = Indexed $ \_ _ -> b
  {-# INLINE return #-}
  Indexed f >>= k = Indexed $ \i a -> runIndexed (k (f i a)) i a
  {-# INLINE (>>=) #-}

instance MonadFix (Indexed i a) where
  mfix f = Indexed $ \ i a -> let o = runIndexed (f o) i a in o
  {-# INLINE mfix #-}

instance Profunctor (Indexed i) where
  dimap ab cd ibc = Indexed (\i -> cd . runIndexed ibc i . ab)
  {-# INLINE dimap #-}
  lmap ab ibc = Indexed (\i -> runIndexed ibc i . ab)
  {-# INLINE lmap #-}
  rmap bc iab = Indexed (\i -> bc . runIndexed iab i)
  {-# INLINE rmap #-}

instance RepresentableProfunctor (Indexed i) where
  type Rep (Indexed i) = (,) i
  tabulatePro = Indexed . curry
  {-# INLINE tabulatePro #-}
  indexPro = uncurry . runIndexed
  {-# INLINE indexPro #-}

instance CorepresentableProfunctor (Indexed i) where
  type Corep (Indexed i) = (->) i
  cotabulatePro = Indexed . flip
  {-# INLINE cotabulatePro #-}
  coindexPro = flip . runIndexed
  {-# INLINE coindexPro #-}

instance Prismatic (Indexed i) where
  prismatic (Indexed iab) = Indexed (either id . iab)
  {-# INLINE prismatic #-}

instance Lenticular (Indexed i) where
  lenticular (Indexed iab) = Indexed $ \i a -> (a, iab i a)
  {-# INLINE lenticular #-}

instance Category (Indexed i) where
  id = Indexed (const id)
  {-# INLINE id #-}
  Indexed f . Indexed g = Indexed $ \i -> f i . g i
  {-# INLINE (.) #-}

instance Arrow (Indexed i) where
  arr f = Indexed (\_ -> f)
  {-# INLINE arr #-}
  first f = Indexed (Arrow.first . runIndexed f)
  {-# INLINE first #-}
  second f = Indexed (Arrow.second . runIndexed f)
  {-# INLINE second #-}
  Indexed f *** Indexed g = Indexed $ \i -> f i *** g i
  {-# INLINE (***) #-}
  Indexed f &&& Indexed g = Indexed $ \i -> f i &&& g i
  {-# INLINE (&&&) #-}

instance ArrowChoice (Indexed i) where
  left f = Indexed (left . runIndexed f)
  {-# INLINE left #-}
  right f = Indexed (right . runIndexed f)
  {-# INLINE right #-}
  Indexed f +++ Indexed g = Indexed $ \i -> f i +++ g i
  {-# INLINE (+++)  #-}
  Indexed f ||| Indexed g = Indexed $ \i -> f i ||| g i
  {-# INLINE (|||) #-}

instance ArrowApply (Indexed i) where
  app = Indexed $ \ i (f, b) -> runIndexed f i b
  {-# INLINE app #-}

instance ArrowLoop (Indexed i) where
  loop (Indexed f) = Indexed $ \i b -> let (c,d) = f i (b, d) in c
  {-# INLINE loop #-}

instance SelfAdjoint (Indexed i) where
  distrib (Indexed iab) = Indexed (\i fa -> iab i <$> fa)
  {-# INLINE distrib #-}

-- | Using an equality witness to avoid potential overlapping instances
-- and aid dispatch.
instance i ~ j => Indexable i (Indexed j) where
  indexed = runIndexed
  {-# INLINE indexed #-}

-----------------------------------------------------------------------------
-- Measured Internals
-----------------------------------------------------------------------------

-- | Given @'Monoid' m@, we also get:
-- @('Prismatic' q, 'ArrowApply' q, 'ArrowChoice' p, 'ArrowLoop' q)@
class (CorepresentableProfunctor q, Lenticular q) => Measurable m q where
  measured :: (a -> (m, b)) -> q a b

instance Measurable m (->) where
  measured f = snd . f
  {-# INLINE measured #-}

newtype Measured m a b = Measured { runMeasured :: a -> (m, b) }

instance m ~ n => Measurable m (Measured n) where
  measured = Measured

instance Functor (Measured m a) where
  fmap f (Measured g) = Measured (fmap f . g)
  {-# INLINE fmap #-}

instance Monoid m => Applicative (Measured m a) where
  pure b = Measured $ \_ -> (mempty, b)
  {-# INLINE pure #-}
  Measured ff <*> Measured fa = Measured $ \e -> let
      ~(m, f) = ff e
      ~(n, a) = fa e
    in (mappend m n, f a)
  {-# INLINE (<*>) #-}
  Measured fa <* Measured fb = Measured $ \e -> let
     ~(m, a) = fa e
     ~(n, _) = fb e
    in (mappend m n, a)
  {-# INLINE (<*) #-}
  Measured fa *> Measured fb = Measured $ \e -> let 
     ~(m, _) = fa e
     ~(n, b) = fb e
    in (mappend m n, b)
  {-# INLINE (*>) #-}

instance Monoid m => Monad (Measured m a) where
  return b = Measured $ \_ -> (mempty, b)
  {-# INLINE return #-}
  Measured h >>= k = Measured $ \a -> let
      ~(m, b) = h a
      ~(n, c) = runMeasured (k b) a
    in (mappend m n, c)
  {-# INLINE (>>=) #-}

instance Monoid m => MonadReader a (Measured m a) where
  ask = Measured $ (,) mempty
  {-# INLINE ask #-}
  local f (Measured h) = Measured (h . f)
  {-# INLINE local #-}

instance Monoid m => MonadWriter m (Measured m a) where
  tell m = Measured $ \_ -> (m, ())
  {-# INLINE tell #-}
  listen (Measured h) = Measured $ \a -> case h a of
    (m, b) -> (m, (b, m))
  {-# INLINE listen #-}
  pass (Measured h) = Measured $ \ a -> case h a of
    (m, (b, f)) -> (f m, b)
  {-# INLINE pass #-}

instance Monoid m => Category (Measured m) where
  id = Measured ((,) mempty)
  {-# INLINE id #-}
  Measured f . Measured g = Measured $ \a -> case g a of
    (m, b) -> case f b of
      (n, c) -> (mappend n m, c)
  {-# INLINE (.) #-}

instance Profunctor (Measured m) where
  dimap f g (Measured h) = Measured (fmap g . h . f)
  {-# INLINE dimap #-}
  lmap f (Measured h) = Measured (h . f)
  {-# INLINE lmap #-}
  rmap g (Measured h) = Measured (fmap g . h)
  {-# INLINE rmap #-}

instance Lenticular (Measured m) where
  lenticular (Measured h) = Measured $ \a -> fmap ((,) a) (h a)
  {-# INLINE lenticular #-}

instance Monoid m => Prismatic (Measured m) where
  prismatic = Measured . either ((,) mempty) . runMeasured
  {-# INLINE prismatic #-}

instance CorepresentableProfunctor (Measured m) where
  type Corep (Measured m) = (,) m
  cotabulatePro = Measured
  {-# INLINE cotabulatePro #-}
  coindexPro = runMeasured
  {-# INLINE coindexPro #-}

instance Monoid m => Arrow (Measured m) where
  arr f = Measured $ \a -> (mempty, f a)
  {-# INLINE arr #-}
  first (Measured h) = Measured $ \(a,c) -> case h a of
    (m, b) -> (m, (b, c))
  {-# INLINE first #-}
  second (Measured h) = Measured $ \(c,a) -> case h a of
    (m, b) -> (m, (c, b))
  {-# INLINE second #-}
  Measured l *** Measured r = Measured $ \(a,b) -> let
      (m, c) = l a
      (n, d) = r b
    in (mappend m n, (c, d))
  {-# INLINE (***) #-}
  Measured l &&& Measured r = Measured $ \a -> let
      (m, b) = l a
      (n, c) = r a
    in (mappend m n, (b, c))
  {-# INLINE (&&&) #-}

instance Monoid m => ArrowChoice (Measured m) where
  left (Measured h) = Measured $ \e -> case e of
    Left a -> Left <$> h a
    Right b -> (mempty, Right b)
  {-# INLINE left #-}
  right (Measured h) = Measured $ \e -> case e of
    Left a -> (mempty, Left a)
    Right b -> Right <$> h b
  {-# INLINE right #-}
  Measured l +++ Measured r = Measured $ \e -> case e of
    Left a -> Left <$> l a
    Right b -> Right <$> r b
  {-# INLINE (+++) #-}
  Measured l ||| Measured r = Measured (either l r)
  {-# INLINE (|||) #-}

instance Monoid m => ArrowApply (Measured m) where
  app = Measured $ \(Measured f, a) -> f a
  {-# INLINE app #-}

instance Monoid m => ArrowLoop (Measured m) where
  loop (Measured f) = Measured $ \ b -> let (m, (c, d)) = f (b,d) in (m, c)
  {-# INLINE loop #-}

instance Monoid m => MonadFix (Measured m a) where
  mfix f = Measured $ \ a -> let (m, o) = runMeasured (f o) a in (m, o)
  {-# INLINE mfix #-}

------------------------------------------------------------------------------
-- Sellable
------------------------------------------------------------------------------

class RepresentableProfunctor p => Sellable p k | k -> p where
  sell :: p a (k a b b)

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
COMPOSE(Size, Int, Size, getSize)
COMPOSE(Size64, Int64, Size64, getSize64)
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
COMPOSE(Identity a, a, Identity, runIdentity)
COMPOSE(Backwards f a, f a, Backwards, forwards)
COMPOSE(Compose f g a, f (g a), Compose, getCompose)
COMPOSE(Cokleisli f a b, f a -> b, Cokleisli, runCokleisli)
COMPOSE(Indexed i s t, i -> s -> t, Indexed, runIndexed)
COMPOSE(Review a b, b, Review, runReview)

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
  {-# INLINE fmap #-}

instance (Monad m, Monoid s) => Applicative (Focusing m s) where
  pure a = Focusing (return (mempty, a))
  {-# INLINE pure #-}
  Focusing mf <*> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (mappend s s', f a)
  {-# INLINE (<*>) #-}

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.RWS.RWST'
newtype FocusingWith w m s a = FocusingWith { unfocusingWith :: m (s, a, w) }

instance Monad m => Functor (FocusingWith w m s) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (s, a, w) <- m
     return (s, f a, w)
  {-# INLINE fmap #-}

instance (Monad m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure a = FocusingWith (return (mempty, a, mempty))
  {-# INLINE pure #-}
  FocusingWith mf <*> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (mappend s s', f a, mappend w w')
  {-# INLINE (<*>) #-}

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)
  {-# INLINE fmap #-}

instance (Monoid w, Applicative (k (s, w))) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  {-# INLINE pure #-}
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)
  {-# INLINE (<*>) #-}

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'
newtype FocusingOn f k s a = FocusingOn { unfocusingOn :: k (f s) a }

instance Functor (k (f s)) => Functor (FocusingOn f k s) where
  fmap f (FocusingOn as) = FocusingOn (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (f s)) => Applicative (FocusingOn f k s) where
  pure = FocusingOn . pure
  {-# INLINE pure #-}
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)
  {-# INLINE (<*>) #-}

-- | Make a monoid out of 'Maybe' for error handling
newtype May a = May { getMay :: Maybe a }

instance Monoid a => Monoid (May a) where
  mempty = May (Just mempty)
  {-# INLINE mempty #-}
  May Nothing `mappend` _ = May Nothing
  _ `mappend` May Nothing = May Nothing
  May (Just a) `mappend` May (Just b) = May (Just (mappend a b))
  {-# INLINE mappend #-}

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingMay k s a = FocusingMay { unfocusingMay :: k (May s) a }

instance Functor (k (May s)) => Functor (FocusingMay k s) where
  fmap f (FocusingMay as) = FocusingMay (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (May s)) => Applicative (FocusingMay k s) where
  pure = FocusingMay . pure
  {-# INLINE pure #-}
  FocusingMay kf <*> FocusingMay ka = FocusingMay (kf <*> ka)
  {-# INLINE (<*>) #-}

-- | Make a monoid out of 'Either' for error handling
newtype Err e a = Err { getErr :: Either e a }

instance Monoid a => Monoid (Err e a) where
  mempty = Err (Right mempty)
  {-# INLINE mempty #-}
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))
  {-# INLINE mappend #-}

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingErr e k s a = FocusingErr { unfocusingErr :: k (Err e s) a }

instance Functor (k (Err e s)) => Functor (FocusingErr e k s) where
  fmap f (FocusingErr as) = FocusingErr (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (Err e s)) => Applicative (FocusingErr e k s) where
  pure = FocusingErr . pure
  {-# INLINE pure #-}
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)
  {-# INLINE (<*>) #-}

-- | Applicative composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

-- | Transform a 'Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- @
-- 'indexing' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.IndexedMeasuredTraversal' 'Int' ('Sum' 'Int') s t a b
-- 'indexing' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.IndexedMeasuredTraversal' 'Int' ('Sum' 'Int') s t a b
-- 'indexing' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.IndexedMeasuredLens' 'Int' ('Sum' 'Int') s t a b
-- 'indexing' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.IndexedMeasuredLens' 'Int' ('Sum' 'Int') s t a b
-- 'indexing' :: 'Control.Lens.Type.Fold' s t          -> 'Control.Lens.Type.IndexedMeasuredFold' 'Int' ('Sum' 'Int') s t
-- 'indexing' :: 'Control.Lens.Type.Getter' s t        -> 'Control.Lens.Type.IndexedMeasuredGetter' 'Int' ('Sum' 'Int') s t a b
-- @
--
-- @'indexing' :: ('Indexable' 'Int' p, 'Measured' ('Sum' 'Int') q) => 'Control.Lens.Type.LensLike' ('Indexing' f) s t a b -> 'Control.Lens.Type.Overloading' p q f s t a b@
indexing :: (Indexable Int p, Measurable Size q) => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> q s (f t)
indexing l iafb = measured $ \ s -> case runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0 of
  ~(i, r) -> (Size i, r)
{-# INLINE indexing #-}

-- | Applicative composition of @'Control.Monad.Trans.State.Lazy.State' 'Int64'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed64'
newtype Indexing64 f a = Indexing64 { runIndexing64 :: Int64 -> (Int64, f a) }

instance Functor f => Functor (Indexing64 f) where
  fmap f (Indexing64 m) = Indexing64 $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing64 f) where
  pure x = Indexing64 $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing64 mf <*> Indexing64 ma = Indexing64 $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

-- | Transform a 'Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- This combinator is like 'indexing' except that it handles large 'Traversal's and 'Fold's gracefully.
--
-- @
-- 'indexing64' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.IndexedMeasuredTraversal' 'Int64' ('Sum' 'Int64') s t a b
-- 'indexing64' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.IndexedMeasuredTraversal' 'Int64' ('Sum' 'Int64') s t a b
-- 'indexing64' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.IndexedMeasuredLens' 'Int64' ('Sum' 'Int64') s t a b
-- 'indexing64' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.IndexedMeasuredLens' 'Int64' ('Sum' 'Int64') s t a b
-- 'indexing64' :: 'Control.Lens.Type.Fold' s t          -> 'Control.Lens.Type.IndexedMeasuredFold' 'Int64' ('Sum' 'Int64') s t
-- 'indexing64' :: 'Control.Lens.Type.Getter' s t        -> 'Control.Lens.Type.IndexedMeasuredGetter' 'Int64' ('Sum' 'Int64') s t a b
-- @
--
-- @'indexing64' :: ('Indexable' 'Int64' p, 'Measured' ('Sum' 'Int64') q) => 'Control.Lens.Type.LensLike' ('Indexing64' f) s t a b -> 'Control.Lens.Type.Overloading' p q f s t a b@
indexing64 :: (Indexable Int64 p, Measurable Size64 q) => ((a -> Indexing64 f b) -> s -> Indexing64 f t) -> p a (f b) -> q s (f t)
indexing64 l iafb = measured $ \ s -> case runIndexing64 (l (\a -> Indexing64 (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0 of
  ~(i, r) -> (Size64 i, r)
{-# INLINE indexing64 #-}

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
newtype Traversed f = Traversed { getTraversed :: f () }

instance Applicative f => Monoid (Traversed f) where
  mempty = Traversed (pure ())
  {-# INLINE mempty #-}
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mappend #-}

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
newtype Sequenced m = Sequenced { getSequenced :: m () }

instance Monad m => Monoid (Sequenced m) where
  mempty = Sequenced (return ())
  {-# INLINE mempty #-}
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE mappend #-}

-- | Used for 'Control.Lens.Fold.minimumOf'
data Min a = NoMin | Min a

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  {-# INLINE mempty #-}
  mappend NoMin m = m
  mappend m NoMin = m
  mappend (Min a) (Min b) = Min (min a b)
  {-# INLINE mappend #-}

-- | Obtain the minimum.
getMin :: Min a -> Maybe a
getMin NoMin   = Nothing
getMin (Min a) = Just a
{-# INLINE getMin #-}

-- | Used for 'Control.Lens.Fold.maximumOf'
data Max a = NoMax | Max a

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  {-# INLINE mempty #-}
  mappend NoMax m = m
  mappend m NoMax = m
  mappend (Max a) (Max b) = Max (max a b)
  {-# INLINE mappend #-}

-- | Obtain the maximum
getMax :: Max a -> Maybe a
getMax NoMax   = Nothing
getMax (Max a) = Just a
{-# INLINE getMax #-}


------------------------------------------------------------------------------
-- Effect
------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }

instance Functor (Effect m r) where
  fmap _ (Effect m) = Effect m
  {-# INLINE fmap #-}

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  {-# INLINE mempty #-}
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)
  {-# INLINE mappend #-}

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  {-# INLINE pure #-}
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)
  {-# INLINE (<*>) #-}

-- | Wrap a monadic effect with a phantom type argument. Used when magnifying RWST.
newtype EffectRWS w st m s a = EffectRWS { getEffectRWS :: st -> m (s,st,w) }

instance Functor (EffectRWS w st m s) where
  fmap _ (EffectRWS m) = EffectRWS m
  {-# INLINE fmap #-}

instance (Monoid s, Monoid w, Monad m) => Applicative (EffectRWS w st m s) where
  pure _ = EffectRWS $ \st -> return (mempty, st, mempty)
  {-# INLINE pure #-}
  EffectRWS m <*> EffectRWS n = EffectRWS $ \st -> m st >>= \ (s,t,w) -> n t >>= \ (s',u,w') -> return (mappend s s', u, mappend w w')
  {-# INLINE (<*>) #-}

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

------------------------------------------------------------------------------
-- Isomorphism and Prism Internals
------------------------------------------------------------------------------

newtype Review a b = Review { runReview :: b }

instance Functor (Review a) where
  fmap bc (Review b) = Review (bc b)
  {-# INLINE fmap #-}

instance Bifunctor Review where
  bimap _ g (Review b) = Review (g b)
  {-# INLINE bimap #-}

instance Profunctor Review where
  dimap _ f (Review c) = Review (f c)
  {-# INLINE dimap #-}
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
  dimap f g x = case runExchange x of
    (sa, bt) -> Exchange (sa . f, g . bt)
  {-# INLINE dimap #-}
  lmap f x = case runExchange x of
    (sa, bt) -> Exchange (sa . f, bt)
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}

newtype Market a b s t = Market { runMarket :: (b -> t, s -> Either t a) }

-- | @type 'Market'' a s t = 'Market' a a s t@
type Market' a = Market a a

instance Functor (Market a b s) where
  fmap f x = case runMarket x of
    (bt, seta) -> Market (f . bt, either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g x = case runMarket x of
    (bt, seta) -> Market (g . bt, either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
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
-- Equality Internals
------------------------------------------------------------------------------

data Identical a b s t where
  Identical :: Identical a b a b

------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------

class IndexedFunctor w where
  ifmap :: (s -> t) -> w a b s -> w a b t

class IndexedFunctor w => IndexedComonad w where
  iextract :: w a a t -> t

  iduplicate :: w a c t -> w a b (w b c t)
  iduplicate = iextend id

  iextend :: (w b c t -> r) -> w a c t -> w a b r
  iextend f = ifmap f . iduplicate

class IndexedComonad w => IndexedComonadStore w where
  ipos :: w a c t -> a

  ipeek :: c  -> w a c t -> t
  ipeek c = iextract . iseek c

  ipeeks :: (a -> c) -> w a c t -> t
  ipeeks f = iextract . iseeks f

  iseek :: b  -> w a c t -> w b c t
  iseeks :: (a -> b) -> w a c t -> w b c t

  iexperiment :: Functor f => (b -> f c) -> w b c t -> f t
  iexperiment bfc wbct = (`ipeek` wbct) <$> bfc (ipos wbct)

  context :: w a b t -> Context a b t
  context wabt = Context (`ipeek` wabt) (ipos wabt)

-- | The indexed store can be used to characterize a 'Control.Lens.Lens.Lens'
-- and is used by 'Control.Lens.Lens.clone'
--
-- @'Context' a b t@ is isomorphic to
-- @newtype Context a b t = Context { runContext :: forall f. Functor f => (a -> f b) -> f t }@,
-- and to @exists s. (s, 'Control.Lens.Lens.Lens' s t a b)@.
--
-- A 'Context' is like a 'Control.Lens.Lens.Lens' that has already been applied to a some structure.
data Context a b t = Context (b -> t) a

instance IndexedFunctor Context where
  ifmap f (Context g t) = Context (f . g) t
  {-# INLINE ifmap #-}

instance IndexedComonad Context where
  iextract   (Context f a) = f a
  {-# INLINE iextract #-}
  iduplicate (Context f a) = Context (Context f) a
  {-# INLINE iduplicate #-}
  iextend g  (Context f a) = Context (g . Context f) a
  {-# INLINE iextend #-}

instance IndexedComonadStore Context where
  ipos (Context _ a) = a
  {-# INLINE ipos #-}
  ipeek b (Context g _) = g b
  {-# INLINE ipeek #-}
  ipeeks f (Context g a) = g (f a)
  {-# INLINE ipeeks #-}
  iseek a (Context g _) = Context g a
  {-# INLINE iseek #-}
  iseeks f (Context g a) = Context g (f a)
  {-# INLINE iseeks #-}
  iexperiment f (Context g a) = g <$> f a
  {-# INLINE iexperiment #-}
  context = id
  {-# INLINE context #-}

instance Functor (Context a b) where
  fmap f (Context g t) = Context (f . g) t
  {-# INLINE fmap #-}

instance (a ~ b) => Comonad (Context a b) where
  extract   (Context f a) = f a
  {-# INLINE extract #-}
  duplicate (Context f a) = Context (Context f) a
  {-# INLINE duplicate #-}
  extend g  (Context f a) = Context (g . Context f) a
  {-# INLINE extend #-}

instance (a ~ b) => ComonadStore a (Context a b) where
  pos = ipos
  {-# INLINE pos #-}
  peek = ipeek
  {-# INLINE peek #-}
  peeks = ipeeks
  {-# INLINE peeks #-}
  seek = iseek
  {-# INLINE seek #-}
  seeks = iseeks
  {-# INLINE seeks #-}
  experiment = iexperiment
  {-# INLINE experiment #-}

instance Sellable (->) Context where
  sell = Context id
  {-# INLINE sell #-}

-- | @type 'Context'' a s = 'Context' a a s@
type Context' a = Context a a

------------------------------------------------------------------------------
-- Bazaar
------------------------------------------------------------------------------
coarr :: (CorepresentableProfunctor q, Comonad (Corep q)) => q a b -> a -> b
coarr qab = extract . coindexPro qab
{-# INLINE coarr #-}

class (Profunctor p, Profunctor q) => Bizarre p q w | w -> p q where
  bazaar :: Applicative f => p a (f b) -> q (w a b t) (f t)

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
-- This is a final encoding of 'Bazaar'.
newtype Bazaar p q a b t = Bazaar { runBazaar :: forall f. Applicative f => p a (f b) `q` f t }

type Bazaar' p q a = Bazaar p q a a

instance Profunctor q => IndexedFunctor (Bazaar p q) where
  ifmap f (Bazaar k) = Bazaar (fmap f `rmap` k)
  {-# INLINE ifmap #-}

instance (SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => IndexedComonad (Bazaar p q) where
  iextract (Bazaar m) = runIdentity $ coarr m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (Bazaar m) = getCompose $ coarr m (rmap Compose (distrib sell) . sell)
  {-# INLINE iduplicate #-}

instance (RepresentableProfunctor p, CorepresentableProfunctor q, Applicative (Corep q))  => Sellable p (Bazaar p q) where
  sell = tabulatePro $ \ w -> Bazaar $ cotabulatePro $ \k -> pure (indexPro k w)
  {-# INLINE sell #-}

instance (Profunctor p, CorepresentableProfunctor q) => Bizarre p q (Bazaar p q) where
  bazaar g = cotabulatePro $ \ f -> coindexPro (runBazaar f) g
  {-# INLINE bazaar #-}

instance Profunctor q => Functor (Bazaar p q a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance (CorepresentableProfunctor q, Applicative (Corep q)) => Applicative (Bazaar p q a b) where
  pure a = Bazaar $ cotabulatePro $ \_ -> pure (pure a)
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar $ cotabulatePro $ \ pafb -> (<*>) <$> coindexPro mf pafb <*> coindexPro ma pafb
  {-# INLINE (<*>) #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => Comonad (Bazaar p q a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => ComonadApply (Bazaar p q a b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

------------------------------------------------------------------------------
-- Pretext
------------------------------------------------------------------------------

newtype Pretext p q a b t = Pretext { runPretext :: forall f. Functor f => p a (f b) `q` f t }

-- | @type 'Pretext'' p q a s = 'Pretext' p q a a s@
type Pretext' p q a = Pretext p q a a

instance Profunctor q => IndexedFunctor (Pretext p q) where
  ifmap f (Pretext k) = Pretext (fmap f `rmap` k)
  {-# INLINE ifmap #-}

instance Profunctor q => Functor (Pretext p q a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance (SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => IndexedComonad (Pretext p q) where
  iextract (Pretext m) = runIdentity $ coarr m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (Pretext m) = getCompose $ coarr m (rmap Compose (distrib sell) . sell)
  {-# INLINE iduplicate #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => Comonad (Pretext p q a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => IndexedComonadStore (Pretext p q) where
  ipos (Pretext m) = getConst $ coarr m $ arr Const
  {-# INLINE ipos #-}
  ipeek a (Pretext m) = runIdentity $ coarr m $ arr (\_ -> Identity a)
  {-# INLINE ipeek #-}
  ipeeks f (Pretext m) = runIdentity $ coarr m $ arr (Identity . f)
  {-# INLINE ipeeks #-}
  iseek a (Pretext m) = Pretext (lmap (lmap (const a)) m)
  {-# INLINE iseek #-}
  iseeks f (Pretext m) = Pretext (lmap (lmap f) m)
  {-# INLINE iseeks #-}
  iexperiment f (Pretext m) = coarr m (arr f)
  {-# INLINE iexperiment #-}
  context (Pretext m) = coarr m (arr sell)
  {-# INLINE context #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => ComonadStore a (Pretext p q a b) where
  pos = ipos
  {-# INLINE pos #-}
  peek = ipeek
  {-# INLINE peek #-}
  peeks = ipeeks
  {-# INLINE peeks #-}
  seek = iseek
  {-# INLINE seek #-}
  seeks = iseeks
  {-# INLINE seeks #-}
  experiment = iexperiment
  {-# INLINE experiment #-}

instance (RepresentableProfunctor p, CorepresentableProfunctor q, Applicative (Corep q))  => Sellable p (Pretext p q) where
  sell = tabulatePro $ \ w -> Pretext $ cotabulatePro $ \k -> pure (indexPro k w)
  {-# INLINE sell #-}

------------------------------------------------------------------------------
-- PretextT
------------------------------------------------------------------------------

newtype PretextT p q (g :: * -> *) a b t = PretextT { runPretextT :: forall f. Functor f => p a (f b) `q` f t }

-- | @type 'PretextT'' p q g a s = 'PretextT' p q g a a s@
type PretextT' p q g a = PretextT p q g a a

instance Profunctor q => IndexedFunctor (PretextT p q g) where
  ifmap f (PretextT k) = PretextT (fmap f `rmap` k)
  {-# INLINE ifmap #-}

instance Profunctor q => Functor (PretextT p q g a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance (SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => IndexedComonad (PretextT p q g) where
  iextract (PretextT m) = runIdentity $ coarr m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (PretextT m) = getCompose $ coarr m (rmap Compose (distrib sell) . sell)
  {-# INLINE iduplicate #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => Comonad (PretextT p q g a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => IndexedComonadStore (PretextT p q g) where
  ipos (PretextT m) = getConst $ coarr m $ arr Const
  {-# INLINE ipos #-}
  ipeek a (PretextT m) = runIdentity $ coarr m $ arr (\_ -> Identity a)
  {-# INLINE ipeek #-}
  ipeeks f (PretextT m) = runIdentity $ coarr m $ arr (Identity . f)
  {-# INLINE ipeeks #-}
  iseek a (PretextT m) = PretextT (lmap (lmap (const a)) m)
  {-# INLINE iseek #-}
  iseeks f (PretextT m) = PretextT (lmap (lmap f) m)
  {-# INLINE iseeks #-}
  iexperiment f (PretextT m) = coarr m (arr f)
  {-# INLINE iexperiment #-}
  context (PretextT m) = coarr m (arr sell)
  {-# INLINE context #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => ComonadStore a (PretextT p q g a b) where
  pos = ipos
  {-# INLINE pos #-}
  peek = ipeek
  {-# INLINE peek #-}
  peeks = ipeeks
  {-# INLINE peeks #-}
  seek = iseek
  {-# INLINE seek #-}
  seeks = iseeks
  {-# INLINE seeks #-}
  experiment = iexperiment
  {-# INLINE experiment #-}

instance (RepresentableProfunctor p, CorepresentableProfunctor q, Applicative (Corep q))  => Sellable p (PretextT p q g) where
  sell = tabulatePro $ \ w -> PretextT $ cotabulatePro $ \k -> pure (indexPro k w)
  {-# INLINE sell #-}

instance (Profunctor p, Profunctor q, Gettable g) => Gettable (PretextT p q g a b) where
  coerce = (<$) (error "coerced PretextT")
  {-# INLINE coerce #-}

------------------------------------------------------------------------------
-- BazaarT
------------------------------------------------------------------------------

-- | 'BazaarT' is like 'Bazaar', except that it provides a questionable 'Gettable' instance
-- To protect this instance it relies on the soundness of another 'Gettable' type, and usage conventions.
--
-- For example. This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there
-- must be a better way!

newtype BazaarT p q (g :: * -> *) a b t = BazaarT { runBazaarT :: forall f. Applicative f => p a (f b) `q` f t }

type BazaarT' p q g a = BazaarT p q g a a

instance Profunctor q => IndexedFunctor (BazaarT p q g) where
  ifmap f (BazaarT k) = BazaarT (fmap f `rmap` k)
  {-# INLINE ifmap #-}

instance (SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => IndexedComonad (BazaarT p q g) where
  iextract (BazaarT m) = runIdentity $ coarr m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (BazaarT m) = getCompose $ coarr m (rmap Compose (distrib sell) . sell)
  {-# INLINE iduplicate #-}

instance (RepresentableProfunctor p, CorepresentableProfunctor q, Applicative (Corep q))  => Sellable p (BazaarT p q g) where
  sell = tabulatePro $ \ w -> BazaarT $ cotabulatePro $ \k -> pure (indexPro k w)
  {-# INLINE sell #-}

instance (Profunctor p, CorepresentableProfunctor q) => Bizarre p q (BazaarT p q g) where
  bazaar g = cotabulatePro $ \ f -> coindexPro (runBazaarT f) g
  {-# INLINE bazaar #-}

instance Profunctor q => Functor (BazaarT p q g a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance (CorepresentableProfunctor q, Applicative (Corep q)) => Applicative (BazaarT p q g a b) where
  pure a = BazaarT $ cotabulatePro $ \_ -> pure (pure a)
  {-# INLINE pure #-}
  BazaarT mf <*> BazaarT ma = BazaarT $ cotabulatePro $ \ pafb -> (<*>) <$> coindexPro mf pafb <*> coindexPro ma pafb
  {-# INLINE (<*>) #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => Comonad (BazaarT p q g a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, SelfAdjoint p, CorepresentableProfunctor q, Comonad (Corep q), Applicative (Corep q)) => ComonadApply (BazaarT p q g a b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

instance (Profunctor p, Profunctor q, Gettable g) => Gettable (BazaarT p q g a b) where
  coerce = (<$) (error "coerced BazaarT")
  {-# INLINE coerce #-}

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

newtype Size = Size { getSize :: Int }
  deriving (Eq,Ord,Show,Read)

instance Monoid Size where
  mempty = Size 0
  mappend (Size m) (Size n) = Size (m + n)

newtype Size64 = Size64 { getSize64 :: Int64 }
  deriving (Eq,Ord,Show,Read)

instance Monoid Size64 where
  mempty = Size64 0
  mappend (Size64 m) (Size64 n) = Size64 (m + n)

-------------------------------------------------------------------------------
-- Orphan Instances
-------------------------------------------------------------------------------

instance Foldable ((,) b) where
  foldMap f (_, a) = f a

instance Traversable ((,) b) where
  traverse f (b, a) = (,) b <$> f a

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right a) = f a

instance Traversable (Either a) where
  traverse _ (Left b) = pure (Left b)
  traverse f (Right a) = Right <$> f a
