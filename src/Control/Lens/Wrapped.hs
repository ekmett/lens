{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Wrapped
-- Copyright   :  (C) 2012-15 Edward Kmett, Michael Sloan
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2, MPTCs, fundeps
--
-- The 'Wrapped' class provides similar functionality as @Control.Newtype@,
-- from the @newtype@ package, but in a more convenient and efficient form.
--
-- There are a few functions from @newtype@ that are not provided here, because
-- they can be done with the 'Iso' directly:
--
-- @
-- Control.Newtype.over 'Sum' f ≡ '_Unwrapping' 'Sum' 'Control.Lens.Setter.%~' f
-- Control.Newtype.under 'Sum' f ≡ '_Wrapping' 'Sum' 'Control.Lens.Setter.%~' f
-- Control.Newtype.overF 'Sum' f ≡ 'mapping' ('_Unwrapping' 'Sum') 'Control.Lens.Setter.%~' f
-- Control.Newtype.underF 'Sum' f ≡ 'mapping' ('_Wrapping' 'Sum') 'Control.Lens.Setter.%~' f
-- @
--
-- 'under' can also be used with '_Unwrapping' to provide the equivalent of
-- @Control.Newtype.under@.  Also, most use cases don't need full polymorphism,
-- so only the single constructor '_Wrapping' functions would be needed.
--
-- These equivalences aren't 100% honest, because @newtype@'s operators
-- need to rely on two @Newtype@ constraints.  This means that the wrapper used
-- for the output is not necessarily the same as the input.
--
----------------------------------------------------------------------------
module Control.Lens.Wrapped
  (
  -- * Wrapping and Unwrapping monomorphically
    Wrapped(..)
  , _Unwrapped'
  , _Wrapping', _Unwrapping'
  -- * Wrapping and unwrapping polymorphically
  , Rewrapped, Rewrapping
  , _Wrapped, _Unwrapped
  , _Wrapping, _Unwrapping
  -- * Operations
  , op
  , ala, alaf
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Applicative.Backwards
import           Control.Comonad.Trans.Traced
import           Control.Exception
import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy      as Lazy
import qualified Control.Monad.Trans.RWS.Strict    as Strict
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Data.Foldable as Foldable
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import qualified Data.Functor.Contravariant.Compose as Contravariant
import           Data.Functor.Constant
import           Data.Functor.Coproduct
import           Data.Functor.Identity
import           Data.Functor.Reverse
import           Data.Hashable
import           Data.IntSet as IntSet
import           Data.IntMap as IntMap
import           Data.HashSet as HashSet
import           Data.HashMap.Lazy as HashMap
import           Data.List.NonEmpty
import           Data.Map as Map
import           Data.Monoid
import qualified Data.Semigroup as S
import           Data.Sequence as Seq hiding (length)
import           Data.Set as Set
import           Data.Tagged
import           Data.Vector as Vector
import           Data.Vector.Primitive as Prim
import           Data.Vector.Unboxed as Unboxed
import           Data.Vector.Storable as Storable

#ifdef HLINT
{-# ANN module "HLint: ignore Use uncurry" #-}
#endif

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | 'Wrapped' provides isomorphisms to wrap and unwrap newtypes or
-- data types with one constructor.
class Wrapped s where
  type Unwrapped s :: *
  -- | An isomorphism between @s@ and @a@.
  _Wrapped' :: Iso' s (Unwrapped s)

-- This can be used to help inference between the wrappers
class Wrapped s => Rewrapped (s :: *) (t :: *)

class    (Rewrapped s t, Rewrapped t s) => Rewrapping s t
instance (Rewrapped s t, Rewrapped t s) => Rewrapping s t

_Unwrapped' :: Wrapped s => Iso' (Unwrapped s) s
_Unwrapped' = from _Wrapped'

-- | Work under a newtype wrapper.
--
-- >>> Const "hello" & _Wrapped %~ Prelude.length & getConst
-- 5
--
-- @
-- '_Wrapped'   ≡ 'from' '_Unwrapped'
-- '_Unwrapped' ≡ 'from' '_Wrapped'
-- @
_Wrapped :: Rewrapping s t => Iso s t (Unwrapped s) (Unwrapped t)
_Wrapped = withIso _Wrapped' $ \ sa _ -> withIso _Wrapped' $ \ _ bt -> iso sa bt
{-# INLINE _Wrapped #-}

_Unwrapped :: Rewrapping s t => Iso (Unwrapped t) (Unwrapped s) t s
_Unwrapped = from _Wrapped
{-# INLINE _Unwrapped #-}

-- * base

instance (t ~ All) => Rewrapped All t
instance Wrapped All where
  type Unwrapped All = Bool
  _Wrapped' = iso getAll All
  {-# INLINE _Wrapped' #-}

instance (t ~ Any) => Rewrapped Any t
instance Wrapped Any where
  type Unwrapped Any = Bool
  _Wrapped' = iso getAny Any
  {-# INLINE _Wrapped' #-}

instance (t ~ Sum b) => Rewrapped (Sum a) t
instance Wrapped (Sum a) where
  type Unwrapped (Sum a) = a
  _Wrapped' = iso getSum Sum
  {-# INLINE _Wrapped' #-}

instance (t ~ Product b) => Rewrapped (Product a) t
instance Wrapped (Product a) where
  type Unwrapped (Product a) = a
  _Wrapped' = iso getProduct Product
  {-# INLINE _Wrapped' #-}

instance (t ~ Kleisli m' a' b') => Rewrapped (Kleisli m a b) t
instance Wrapped (Kleisli m a b) where
  type Unwrapped (Kleisli m a b) = a -> m b
  _Wrapped' = iso runKleisli Kleisli
  {-# INLINE _Wrapped' #-}

instance (t ~ WrappedMonad m' a') => Rewrapped (WrappedMonad m a) t
instance Wrapped (WrappedMonad m a) where
  type Unwrapped (WrappedMonad m a) = m a
  _Wrapped' = iso unwrapMonad WrapMonad
  {-# INLINE _Wrapped' #-}

instance (t ~ WrappedArrow a' b' c') => Rewrapped (WrappedArrow a b c) t
instance Wrapped (WrappedArrow a b c) where
  type Unwrapped (WrappedArrow a b c) = a b c
  _Wrapped' = iso unwrapArrow WrapArrow
  {-# INLINE _Wrapped' #-}

instance (t ~ ZipList b) => Rewrapped (ZipList a) t
instance Wrapped (ZipList a) where
  type Unwrapped (ZipList a) = [a]
  _Wrapped' = iso getZipList ZipList
  {-# INLINE _Wrapped' #-}

instance (t ~ NonEmpty b) => Rewrapped (NonEmpty a) t
instance Wrapped (NonEmpty a) where
  type Unwrapped (NonEmpty a) = (a, [a])
  _Wrapped' = iso (\(a :| as) -> (a, as)) (\(a,as) -> a :| as)
  {-# INLINE _Wrapped' #-}

instance (t ~ Const a' x') => Rewrapped (Const a x) t
instance Wrapped (Const a x) where
  type Unwrapped (Const a x) = a
  _Wrapped' = iso getConst Const
  {-# INLINE _Wrapped' #-}

instance (t ~ Dual b) => Rewrapped (Dual a) t
instance Wrapped (Dual a) where
  type Unwrapped (Dual a) = a
  _Wrapped' = iso getDual Dual
  {-# INLINE _Wrapped' #-}

instance (t ~ Endo b) => Rewrapped (Endo b) t
instance Wrapped (Endo a) where
  type Unwrapped (Endo a) = a -> a
  _Wrapped' = iso appEndo Endo
  {-# INLINE _Wrapped' #-}

instance (t ~ First b) => Rewrapped (First a) t
instance Wrapped (First a) where
  type Unwrapped (First a) = Maybe a
  _Wrapped' = iso getFirst First
  {-# INLINE _Wrapped' #-}

instance (t ~ Last b) => Rewrapped (Last a) t
instance Wrapped (Last a) where
  type Unwrapped (Last a) = Maybe a
  _Wrapped' = iso getLast Last
  {-# INLINE _Wrapped' #-}

#if MIN_VERSION_base(4,8,0)
instance (t ~ Alt g b) => Rewrapped (Alt f a) t
instance Wrapped (Alt f a) where
  type Unwrapped (Alt f a) = f a
  _Wrapped' = iso getAlt Alt
  {-# INLINE _Wrapped' #-}
#endif

instance (t ~ ArrowMonad m' a', ArrowApply m) => Rewrapped (ArrowMonad m a) t
instance Wrapped (ArrowMonad m a) where
  type Unwrapped (ArrowMonad m a) = m () a
  _Wrapped' = iso getArrowMonad ArrowMonad
  {-# INLINE _Wrapped' #-}

-- * transformers

instance (t ~ Backwards g b) => Rewrapped (Backwards f a) t
instance Wrapped (Backwards f a) where
  type Unwrapped (Backwards f a) = f a
  _Wrapped' = iso forwards Backwards

instance (t ~ Compose f' g' a') => Rewrapped (Compose f g a) t
instance Wrapped (Compose f g a) where
  type Unwrapped (Compose f g a) = f (g a)
  _Wrapped' = iso getCompose Compose

instance (t ~ Constant a' b') => Rewrapped (Constant a b) t
instance Wrapped (Constant a b) where
  type Unwrapped (Constant a b) = a
  _Wrapped' = iso getConstant Constant

instance (t ~ ContT r' m' a') => Rewrapped (ContT r m a) t
instance Wrapped (ContT r m a) where
  type Unwrapped (ContT r m a) = (a -> m r) -> m r
  _Wrapped' = iso runContT ContT

instance (t ~ ErrorT e' m' a') => Rewrapped (ErrorT e m a) t
instance Wrapped (ErrorT e m a) where
  type Unwrapped (ErrorT e m a) = m (Either e a)
  _Wrapped' = iso runErrorT ErrorT
  {-# INLINE _Wrapped' #-}

instance (t ~ Identity b) => Rewrapped (Identity a) t
instance Wrapped (Identity a) where
  type Unwrapped (Identity a) = a
  _Wrapped' = iso runIdentity Identity
  {-# INLINE _Wrapped' #-}

instance (t ~ IdentityT n b) => Rewrapped (IdentityT m a) t
instance Wrapped (IdentityT m a) where
  type Unwrapped (IdentityT m a) = m a
  _Wrapped' = iso runIdentityT IdentityT
  {-# INLINE _Wrapped' #-}

instance (t ~ ListT n b) => Rewrapped (ListT m a) t
instance Wrapped (ListT m a) where
  type Unwrapped (ListT m a) = m [a]
  _Wrapped' = iso runListT ListT
  {-# INLINE _Wrapped' #-}

instance (t ~ MaybeT n b) => Rewrapped (MaybeT m a) t
instance Wrapped (MaybeT m a) where
  type Unwrapped (MaybeT m a) = m (Maybe a)
  _Wrapped' = iso runMaybeT MaybeT
  {-# INLINE _Wrapped' #-}

instance (t ~ ReaderT r n b) => Rewrapped (ReaderT r m a) t
instance Wrapped (ReaderT r m a) where
  type Unwrapped (ReaderT r m a) = r -> m a
  _Wrapped' = iso runReaderT ReaderT
  {-# INLINE _Wrapped' #-}

instance (t ~ Reverse g b) => Rewrapped (Reverse f a) t
instance Wrapped (Reverse f a) where
  type Unwrapped (Reverse f a) = f a
  _Wrapped' = iso getReverse Reverse
  {-# INLINE _Wrapped' #-}

instance (t ~ Lazy.RWST r' w' s' m' a') => Rewrapped (Lazy.RWST r w s m a) t
instance Wrapped (Lazy.RWST r w s m a) where
  type Unwrapped (Lazy.RWST r w s m a) = r -> s -> m (a, s, w)
  _Wrapped' = iso Lazy.runRWST Lazy.RWST
  {-# INLINE _Wrapped' #-}

instance (t ~ Strict.RWST r' w' s' m' a') => Rewrapped (Strict.RWST r w s m a) t
instance Wrapped (Strict.RWST r w s m a) where
  type Unwrapped (Strict.RWST r w s m a) = r -> s -> m (a, s, w)
  _Wrapped' = iso Strict.runRWST Strict.RWST
  {-# INLINE _Wrapped' #-}

instance (t ~ Lazy.StateT s' m' a') => Rewrapped (Lazy.StateT s m a) t
instance Wrapped (Lazy.StateT s m a) where
  type Unwrapped (Lazy.StateT s m a) = s -> m (a, s)
  _Wrapped' = iso Lazy.runStateT Lazy.StateT
  {-# INLINE _Wrapped' #-}

instance (t ~ Strict.StateT s' m' a') => Rewrapped (Strict.StateT s m a) t
instance Wrapped (Strict.StateT s m a) where
  type Unwrapped (Strict.StateT s m a) = s -> m (a, s)
  _Wrapped' = iso Strict.runStateT Strict.StateT
  {-# INLINE _Wrapped' #-}

instance (t ~ Lazy.WriterT w' m' a') => Rewrapped (Lazy.WriterT w m a) t
instance Wrapped (Lazy.WriterT w m a) where
  type Unwrapped (Lazy.WriterT w m a) = m (a, w)
  _Wrapped' = iso Lazy.runWriterT Lazy.WriterT
  {-# INLINE _Wrapped' #-}

instance (t ~ Strict.WriterT w' m' a') => Rewrapped (Strict.WriterT w m a) t
instance Wrapped (Strict.WriterT w m a) where
  type Unwrapped (Strict.WriterT w m a) = m (a, w)
  _Wrapped' = iso Strict.runWriterT Strict.WriterT
  {-# INLINE _Wrapped' #-}

-- * comonad-transformers

instance (t ~ Coproduct f' g' a') => Rewrapped (Coproduct f g a) t
instance Wrapped (Coproduct f g a) where
  type Unwrapped (Coproduct f g a) = Either (f a) (g a)
  _Wrapped' = iso getCoproduct Coproduct
  {-# INLINE _Wrapped' #-}

instance (t ~ TracedT m' w' a') => Rewrapped (TracedT m w a) t
instance Wrapped (TracedT m w a) where
  type Unwrapped (TracedT m w a) = w (m -> a)
  _Wrapped' = iso runTracedT TracedT
  {-# INLINE _Wrapped' #-}

-- * unordered-containers

-- | Use @'wrapping' 'HashMap.fromList'@. Unwrapping returns some permutation of the list.
instance (t ~ HashMap k' a', Hashable k, Eq k) => Rewrapped (HashMap k a) t
instance (Hashable k, Eq k) => Wrapped (HashMap k a) where
  type Unwrapped (HashMap k a) = [(k, a)]
  _Wrapped' = iso HashMap.toList HashMap.fromList
  {-# INLINE _Wrapped' #-}

-- | Use @'wrapping' 'HashSet.fromList'@. Unwrapping returns some permutation of the list.
instance (t ~ HashSet a', Hashable a, Eq a) => Rewrapped (HashSet a) t
instance (Hashable a, Eq a) => Wrapped (HashSet a) where
  type Unwrapped (HashSet a) = [a]
  _Wrapped' = iso HashSet.toList HashSet.fromList
  {-# INLINE _Wrapped' #-}

-- * containers

-- | Use @'wrapping' 'IntMap.fromList'@. unwrapping returns a /sorted/ list.
instance (t ~ IntMap a') => Rewrapped (IntMap a) t
instance Wrapped (IntMap a) where
  type Unwrapped (IntMap a) = [(Int, a)]
  _Wrapped' = iso IntMap.toAscList IntMap.fromList
  {-# INLINE _Wrapped' #-}

-- | Use @'wrapping' 'IntSet.fromList'@. unwrapping returns a /sorted/ list.
instance (t ~ IntSet) => Rewrapped IntSet t
instance Wrapped IntSet where
  type Unwrapped IntSet = [Int]
  _Wrapped' = iso IntSet.toAscList IntSet.fromList
  {-# INLINE _Wrapped' #-}

-- | Use @'wrapping' 'Map.fromList'@. unwrapping returns a /sorted/ list.
instance (t ~ Map k' a', Ord k) => Rewrapped (Map k a) t
instance Ord k => Wrapped (Map k a) where
  type Unwrapped (Map k a) = [(k, a)]
  _Wrapped' = iso Map.toAscList Map.fromList
  {-# INLINE _Wrapped' #-}

-- | Use @'wrapping' 'Set.fromList'@. unwrapping returns a /sorted/ list.
instance (t ~ Set a', Ord a) => Rewrapped (Set a) t
instance Ord a => Wrapped (Set a) where
  type Unwrapped (Set a) = [a]
  _Wrapped' = iso Set.toAscList Set.fromList
  {-# INLINE _Wrapped' #-}

instance (t ~ Seq a') => Rewrapped (Seq a) t
instance Wrapped (Seq a) where
  type Unwrapped (Seq a) = [a]
  _Wrapped' = iso Foldable.toList Seq.fromList
  {-# INLINE _Wrapped' #-}

-- * vector

instance (t ~ Vector.Vector a') => Rewrapped (Vector.Vector a) t
instance Wrapped (Vector.Vector a) where
  type Unwrapped (Vector.Vector a) = [a]
  _Wrapped' = iso Vector.toList Vector.fromList
  {-# INLINE _Wrapped' #-}

instance (Prim a, t ~ Prim.Vector a') => Rewrapped (Prim.Vector a) t
instance Prim a => Wrapped (Prim.Vector a) where
  type Unwrapped (Prim.Vector a) = [a]
  _Wrapped' = iso Prim.toList Prim.fromList
  {-# INLINE _Wrapped' #-}

instance (Unbox a, t ~ Unboxed.Vector a') => Rewrapped (Unboxed.Vector a) t
instance Unbox a => Wrapped (Unboxed.Vector a) where
  type Unwrapped (Unboxed.Vector a) = [a]
  _Wrapped' = iso Unboxed.toList Unboxed.fromList
  {-# INLINE _Wrapped' #-}

instance (Storable a, t ~ Storable.Vector a') => Rewrapped (Storable.Vector a) t
instance Storable a => Wrapped (Storable.Vector a) where
  type Unwrapped (Storable.Vector a) = [a]
  _Wrapped' = iso Storable.toList Storable.fromList
  {-# INLINE _Wrapped' #-}

-- * semigroups

instance (t ~ S.Min b) => Rewrapped (S.Min a) t
instance Wrapped (S.Min a) where
  type Unwrapped (S.Min a) = a
  _Wrapped' = iso S.getMin S.Min
  {-# INLINE _Wrapped' #-}

instance (t ~ S.Max b) => Rewrapped (S.Max a) t
instance Wrapped (S.Max a) where
  type Unwrapped (S.Max a) = a
  _Wrapped' = iso S.getMax S.Max
  {-# INLINE _Wrapped' #-}

instance (t ~ S.First b) => Rewrapped (S.First a) t
instance Wrapped (S.First a) where
  type Unwrapped (S.First a) = a
  _Wrapped' = iso S.getFirst S.First
  {-# INLINE _Wrapped' #-}

instance (t ~ S.Last b) => Rewrapped (S.Last a) t
instance Wrapped (S.Last a) where
  type Unwrapped (S.Last a) = a
  _Wrapped' = iso S.getLast S.Last
  {-# INLINE _Wrapped' #-}

instance (t ~ S.WrappedMonoid b) => Rewrapped (S.WrappedMonoid a) t
instance Wrapped (S.WrappedMonoid a) where
  type Unwrapped (S.WrappedMonoid a) = a
  _Wrapped' = iso S.unwrapMonoid S.WrapMonoid
  {-# INLINE _Wrapped' #-}

instance (t ~ S.Option b) => Rewrapped (S.Option a) t
instance Wrapped (S.Option a) where
  type Unwrapped (S.Option a) = Maybe a
  _Wrapped' = iso S.getOption S.Option
  {-# INLINE _Wrapped' #-}

-- * contravariant

instance (t ~ Predicate b) => Rewrapped (Predicate a) t
instance Wrapped (Predicate a) where
  type Unwrapped (Predicate a) = a -> Bool
  _Wrapped' = iso getPredicate Predicate
  {-# INLINE _Wrapped' #-}

instance (t ~ Comparison b) => Rewrapped (Comparison a) t
instance Wrapped (Comparison a) where
  type Unwrapped (Comparison a) = a -> a -> Ordering
  _Wrapped' = iso getComparison Comparison
  {-# INLINE _Wrapped' #-}

instance (t ~ Equivalence b) => Rewrapped (Equivalence a) t
instance Wrapped (Equivalence a) where
  type Unwrapped (Equivalence a) = a -> a -> Bool
  _Wrapped' = iso getEquivalence Equivalence
  {-# INLINE _Wrapped' #-}

instance (t ~ Op a' b') => Rewrapped (Op a b) t
instance Wrapped (Op a b) where
  type Unwrapped (Op a b) = b -> a
  _Wrapped' = iso getOp Op
  {-# INLINE _Wrapped' #-}

instance (t ~ Contravariant.Compose f' g' a') => Rewrapped (Contravariant.Compose f g a) t
instance Wrapped (Contravariant.Compose f g a) where
  type Unwrapped (Contravariant.Compose f g a) = f (g a)
  _Wrapped' = iso Contravariant.getCompose Contravariant.Compose
  {-# INLINE _Wrapped' #-}

instance (t ~ Contravariant.ComposeFC f' g' a') => Rewrapped (Contravariant.ComposeFC f g a) t
instance Wrapped (Contravariant.ComposeFC f g a) where
  type Unwrapped (Contravariant.ComposeFC f g a) = f (g a)
  _Wrapped' = iso Contravariant.getComposeFC Contravariant.ComposeFC
  {-# INLINE _Wrapped' #-}

instance (t ~ Contravariant.ComposeCF f' g' a') => Rewrapped (Contravariant.ComposeCF f g a) t
instance Wrapped (Contravariant.ComposeCF f g a) where
  type Unwrapped (Contravariant.ComposeCF f g a) = f (g a)
  _Wrapped' = iso Contravariant.getComposeCF Contravariant.ComposeCF
  {-# INLINE _Wrapped' #-}

-- * tagged

instance (t ~ Tagged s' a') => Rewrapped (Tagged s a) t
instance Wrapped (Tagged s a) where
  type Unwrapped (Tagged s a) = a
  _Wrapped' = iso unTagged Tagged
  {-# INLINE _Wrapped' #-}

-- * Control.Exception

instance (t ~ AssertionFailed) => Rewrapped AssertionFailed t
instance Wrapped AssertionFailed where
  type Unwrapped AssertionFailed = String
  _Wrapped' = iso failedAssertion AssertionFailed
  {-# INLINE _Wrapped' #-}

instance (t ~ NoMethodError) => Rewrapped NoMethodError t
instance Wrapped NoMethodError where
  type Unwrapped NoMethodError = String
  _Wrapped' = iso getNoMethodError NoMethodError
  {-# INLINE _Wrapped' #-}

instance (t ~ PatternMatchFail) => Rewrapped PatternMatchFail t
instance Wrapped PatternMatchFail where
  type Unwrapped PatternMatchFail = String
  _Wrapped' = iso getPatternMatchFail PatternMatchFail
  {-# INLINE _Wrapped' #-}

instance (t ~ RecConError) => Rewrapped RecConError t
instance Wrapped RecConError where
  type Unwrapped RecConError = String
  _Wrapped' = iso getRecConError RecConError
  {-# INLINE _Wrapped' #-}

instance (t ~ RecSelError) => Rewrapped RecSelError t
instance Wrapped RecSelError where
  type Unwrapped RecSelError = String
  _Wrapped' = iso getRecSelError RecSelError
  {-# INLINE _Wrapped' #-}

instance (t ~ RecUpdError) => Rewrapped RecUpdError t
instance Wrapped RecUpdError where
  type Unwrapped RecUpdError = String
  _Wrapped' = iso getRecUpdError RecUpdError
  {-# INLINE _Wrapped' #-}

instance (t ~ ErrorCall) => Rewrapped ErrorCall t
instance Wrapped ErrorCall where
  type Unwrapped ErrorCall = String
  _Wrapped' = iso getErrorCall ErrorCall
  {-# INLINE _Wrapped' #-}

getErrorCall :: ErrorCall -> String
getErrorCall (ErrorCall x) = x
{-# INLINE getErrorCall #-}

getRecUpdError :: RecUpdError -> String
getRecUpdError (RecUpdError x) = x
{-# INLINE getRecUpdError #-}

getRecSelError :: RecSelError -> String
getRecSelError (RecSelError x) = x
{-# INLINE getRecSelError #-}

getRecConError :: RecConError -> String
getRecConError (RecConError x) = x
{-# INLINE getRecConError #-}

getPatternMatchFail :: PatternMatchFail -> String
getPatternMatchFail (PatternMatchFail x) = x
{-# INLINE getPatternMatchFail #-}

getNoMethodError :: NoMethodError -> String
getNoMethodError (NoMethodError x) = x
{-# INLINE getNoMethodError #-}

failedAssertion :: AssertionFailed -> String
failedAssertion (AssertionFailed x) = x
{-# INLINE failedAssertion #-}

getArrowMonad :: ArrowMonad m a -> m () a
getArrowMonad (ArrowMonad x) = x
{-# INLINE getArrowMonad #-}

-- | Given the constructor for a 'Wrapped' type, return a
-- deconstructor that is its inverse.
--
-- Assuming the 'Wrapped' instance is legal, these laws hold:
--
-- @
-- 'op' f '.' f ≡ 'id'
-- f '.' 'op' f ≡ 'id'
-- @
--
--
-- >>> op Identity (Identity 4)
-- 4
--
-- >>> op Const (Const "hello")
-- "hello"
op :: Wrapped s => (Unwrapped s -> s) -> s -> Unwrapped s
op _ = view _Wrapped'
{-# INLINE op #-}

-- | This is a convenient version of '_Wrapped' with an argument that's ignored.
--
-- The user supplied function is /ignored/, merely its type is used.
_Wrapping' :: Wrapped s => (Unwrapped s -> s) -> Iso' s (Unwrapped s)
_Wrapping' _ = _Wrapped'
{-# INLINE _Wrapping' #-}

-- | This is a convenient version of '_Wrapped' with an argument that's ignored.
--
-- The user supplied function is /ignored/, merely its type is used.
_Unwrapping' :: Wrapped s => (Unwrapped s -> s) -> Iso' (Unwrapped s) s
_Unwrapping' _ = from _Wrapped'
{-# INLINE _Unwrapping' #-}

-- | This is a convenient version of '_Wrapped' with an argument that's ignored.
--
-- The user supplied function is /ignored/, merely its types are used.
_Wrapping :: Rewrapping s t => (Unwrapped s -> s) -> Iso s t (Unwrapped s) (Unwrapped t)
_Wrapping _ = _Wrapped
{-# INLINE _Wrapping #-}

-- | This is a convenient version of '_Unwrapped' with an argument that's ignored.
--
-- The user supplied function is /ignored/, merely its types are used.
_Unwrapping :: Rewrapping s t => (Unwrapped s -> s) -> Iso (Unwrapped t) (Unwrapped s) t s
_Unwrapping _ = from _Wrapped
{-# INLINE _Unwrapping #-}

-- | This combinator is based on @ala@ from Conor McBride's work on Epigram.
--
-- As with '_Wrapping', the user supplied function for the newtype is /ignored/.
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
--
-- >>> ala All foldMap [True,True]
-- True
--
-- >>> ala All foldMap [True,False]
-- False
--
-- >>> ala Any foldMap [False,False]
-- False
--
-- >>> ala Any foldMap [True,False]
-- True
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
--
-- >>> ala Product foldMap [1,2,3,4]
-- 24
ala :: Rewrapping s t => (Unwrapped s -> s) -> ((Unwrapped t -> t) -> e -> s) -> e -> Unwrapped s
ala = au . _Wrapping
{-# INLINE ala #-}

-- | This combinator is based on @ala'@ from Conor McBride's work on Epigram.
--
-- As with '_Wrapping', the user supplied function for the newtype is /ignored/.
--
-- >>> alaf Sum foldMap Prelude.length ["hello","world"]
-- 10
alaf :: (Profunctor p, Rewrapping s t) => (Unwrapped s -> s) -> (p r t -> e -> s) -> p r (Unwrapped t) -> e -> Unwrapped s
alaf = auf . _Unwrapping
{-# INLINE alaf #-}
