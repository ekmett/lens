{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Wrapped
-- Copyright   :  (C) 2012-13 Edward Kmett, Michael Sloan
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
-- Control.Newtype.over 'Sum' f ≡ 'wrapping' 'Sum' 'Control.Lens.Setter.%~' f
-- Control.Newtype.under 'Sum' f ≡ 'unwrapping' 'Sum' 'Control.Lens.Setter.%~' f
-- Control.Newtype.overF 'Sum' f ≡ 'mapping' ('wrapping' 'Sum') 'Control.Lens.Setter.%~' f
-- Control.Newtype.underF 'Sum' f ≡ 'mapping' ('unwrapping' 'Sum') 'Control.Lens.Setter.%~' f
-- @
--
-- 'under' can also be used with 'wrapping' to provide the equivalent of
-- @Control.Newtype.under@.  Also, most use cases don't need full polymorphism,
-- so only the single constructor 'wrapping' functions would be needed.
--
-- These equivalences aren't 100% honest, because @newtype@'s operators
-- need to rely on two @Newtype@ constraints.  This means that the wrapper used
-- for the output is not necessarily the same as the input.
--
----------------------------------------------------------------------------
module Control.Lens.Wrapped
  ( Wrapped(..)
  , unwrapped
  , wrapped', unwrapped'
  , wrapping, unwrapping
  , wrappings, unwrappings
  , op
  , ala, alaf
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Applicative.Backwards
import           Control.Comonad.Trans.Traced
import           Control.Exception
import           Control.Lens.Internal.Review
import           Control.Lens.Iso
import           Control.Lens.Review
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
import           Data.Map as Map
import           Data.Monoid
import           Data.Sequence as Seq hiding (length)
import           Data.Set as Set
import           Data.Tagged

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | 'Wrapped' provides isomorphisms to wrap and unwrap newtypes or
-- data types with one constructor.
class Wrapped s t a b | a -> s, b -> t, a t -> s, b s -> t where
  -- | An isomorphism between s and @a@ and a related one between @t@ and @b@, such that when @a = b@, @s = t@.
  --
  -- This is often used via 'wrapping' to aid type inference.
  wrapped   :: Iso s t a b

-- * base

instance Wrapped Bool Bool All All where
  wrapped = iso All getAll
  {-# INLINE wrapped #-}

instance Wrapped Bool Bool Any Any where
  wrapped = iso Any getAny
  {-# INLINE wrapped #-}

instance Wrapped a b (Sum a) (Sum b) where
  wrapped = iso Sum getSum
  {-# INLINE wrapped #-}

instance Wrapped a b (Product a) (Product b) where
  wrapped = iso Product getProduct
  {-# INLINE wrapped #-}

instance Wrapped (a -> m b) (u -> n v) (Kleisli m a b) (Kleisli n u v) where
  wrapped = iso Kleisli runKleisli
  {-# INLINE wrapped #-}

instance Wrapped (m a) (n b) (WrappedMonad m a) (WrappedMonad n b) where
  wrapped = iso WrapMonad unwrapMonad
  {-# INLINE wrapped #-}

instance Wrapped (a b c) (u v w) (WrappedArrow a b c) (WrappedArrow u v w) where
  wrapped = iso WrapArrow unwrapArrow
  {-# INLINE wrapped #-}

instance Wrapped [a] [b] (ZipList a) (ZipList b) where
  wrapped = iso ZipList getZipList
  {-# INLINE wrapped #-}

instance Wrapped a b (Const a x) (Const b y) where
  wrapped = iso Const getConst
  {-# INLINE wrapped #-}

instance Wrapped a b (Dual a) (Dual b) where
  wrapped = iso Dual getDual
  {-# INLINE wrapped #-}

instance Wrapped (a -> a) (b -> b) (Endo a) (Endo b) where
  wrapped = iso Endo appEndo
  {-# INLINE wrapped #-}

instance Wrapped (Maybe a) (Maybe b) (First a) (First b) where
  wrapped = iso First getFirst
  {-# INLINE wrapped #-}

instance Wrapped (Maybe a) (Maybe b) (Last a) (Last b) where
  wrapped = iso Last getLast
  {-# INLINE wrapped #-}

instance (ArrowApply m, ArrowApply n) => Wrapped (m () a) (n () b) (ArrowMonad m a) (ArrowMonad n b) where
  wrapped = iso ArrowMonad getArrowMonad
  {-# INLINE wrapped #-}

-- * lens

instance Wrapped a b (Reviewed s a) (Reviewed t b) where
  wrapped = iso Reviewed runReviewed
  {-# INLINE wrapped #-}

-- * transformers

instance Wrapped (f a) (f' a') (Backwards f a) (Backwards f' a') where
  wrapped = iso Backwards forwards
  {-# INLINE wrapped #-}

instance Wrapped (f (g a)) (f' (g' a')) (Compose f g a) (Compose f' g' a') where
  wrapped = iso Compose getCompose
  {-# INLINE wrapped #-}

instance Wrapped a a' (Constant a b) (Constant a' b') where
  wrapped = iso Constant getConstant
  {-# INLINE wrapped #-}

instance Wrapped ((a -> m r) -> m r) ((a' -> m' r') -> m' r') (ContT r m a) (ContT r' m' a') where
  wrapped = iso ContT runContT
  {-# INLINE wrapped #-}

instance Wrapped (m (Either e a)) (m' (Either e' a')) (ErrorT e m a) (ErrorT e' m' a') where
  wrapped = iso ErrorT runErrorT
  {-# INLINE wrapped #-}

instance Wrapped a a' (Identity a) (Identity a') where
  wrapped = iso Identity runIdentity
  {-# INLINE wrapped #-}

instance Wrapped (m a) (m' a') (IdentityT m a) (IdentityT m' a') where
  wrapped = iso IdentityT runIdentityT
  {-# INLINE wrapped #-}

instance Wrapped (m [a]) (m' [a']) (ListT m a) (ListT m' a') where
  wrapped = iso ListT runListT
  {-# INLINE wrapped #-}

instance Wrapped (m (Maybe a)) (m' (Maybe a')) (MaybeT m a) (MaybeT m' a') where
  wrapped = iso MaybeT runMaybeT
  {-# INLINE wrapped #-}

instance Wrapped (r -> m a) (r' -> m' a') (ReaderT r m a) (ReaderT r' m' a') where
  wrapped = iso ReaderT runReaderT
  {-# INLINE wrapped #-}

instance Wrapped (f a) (f' a') (Reverse f a) (Reverse f' a') where
  wrapped = iso Reverse getReverse
  {-# INLINE wrapped #-}

instance Wrapped (r -> s -> m (a, s, w)) (r' -> s' -> m' (a', s', w')) (Lazy.RWST r w s m a) (Lazy.RWST r' w' s' m' a') where
  wrapped = iso Lazy.RWST Lazy.runRWST
  {-# INLINE wrapped #-}

instance Wrapped (r -> s -> m (a, s, w)) (r' -> s' -> m' (a', s', w')) (Strict.RWST r w s m a) (Strict.RWST r' w' s' m' a') where
  wrapped = iso Strict.RWST Strict.runRWST
  {-# INLINE wrapped #-}

instance Wrapped (s -> m (a, s)) (s' -> m' (a', s')) (Lazy.StateT s m a) (Lazy.StateT s' m' a') where
  wrapped = iso Lazy.StateT Lazy.runStateT
  {-# INLINE wrapped #-}

instance Wrapped (s -> m (a, s)) (s' -> m' (a', s')) (Strict.StateT s m a) (Strict.StateT s' m' a') where
  wrapped = iso Strict.StateT Strict.runStateT
  {-# INLINE wrapped #-}

instance Wrapped (m (a, w)) (m' (a', w')) (Lazy.WriterT w m a) (Lazy.WriterT w' m' a') where
  wrapped = iso Lazy.WriterT Lazy.runWriterT
  {-# INLINE wrapped #-}

instance Wrapped (m (a, w)) (m' (a', w')) (Strict.WriterT w m a) (Strict.WriterT w' m' a') where
  wrapped = iso Strict.WriterT Strict.runWriterT
  {-# INLINE wrapped #-}

-- * comonad-transformers

instance Wrapped (Either (f a) (g a)) (Either (f' a') (g' a')) (Coproduct f g a) (Coproduct f' g' a') where
  wrapped = iso Coproduct getCoproduct
  {-# INLINE wrapped #-}

instance Wrapped (w (m -> a)) (w' (m' -> a')) (TracedT m w a) (TracedT m' w' a') where
  wrapped = iso TracedT runTracedT
  {-# INLINE wrapped #-}

-- * unordered-containers

-- | Use @'wrapping' 'HashMap.fromList'@. Unwrapping returns some permutation of the list.
instance (Hashable k, Eq k, Hashable k', Eq k') => Wrapped [(k, a)] [(k', b)] (HashMap k a) (HashMap k' b) where
  wrapped = iso HashMap.fromList HashMap.toList
  {-# INLINE wrapped #-}

-- | Use @'wrapping' 'HashSet.fromList'@. Unwrapping returns some permutation of the list.
instance (Hashable a, Eq a, Hashable b, Eq b) => Wrapped [a] [b] (HashSet a) (HashSet b) where
  wrapped = iso HashSet.fromList HashSet.toList
  {-# INLINE wrapped #-}

-- * containers

-- | Use @'wrapping' 'IntMap.fromList'@. unwrapping returns a /sorted/ list.
instance Wrapped [(Int, a)] [(Int, b)] (IntMap a) (IntMap b) where
  wrapped = iso IntMap.fromList IntMap.toAscList
  {-# INLINE wrapped #-}

-- | Use @'wrapping' 'IntSet.fromList'@. unwrapping returns a /sorted/ list.
instance Wrapped [Int] [Int] IntSet IntSet where
  wrapped = iso IntSet.fromList IntSet.toAscList
  {-# INLINE wrapped #-}

-- | Use @'wrapping' 'Map.fromList'@. unwrapping returns a /sorted/ list.
instance (Ord k, Ord k') => Wrapped [(k, a)] [(k', b)] (Map k a) (Map k' b) where
  wrapped = iso Map.fromList Map.toAscList
  {-# INLINE wrapped #-}

-- | Use @'wrapping' 'Set.fromList'@. unwrapping returns a /sorted/ list.
instance (Ord a, Ord b) => Wrapped [a] [b] (Set a) (Set b) where
  wrapped = iso Set.fromList Set.toAscList
  {-# INLINE wrapped #-}

instance Wrapped [a] [b] (Seq a) (Seq b) where
  wrapped = iso Seq.fromList Foldable.toList
  {-# INLINE wrapped #-}

-- * contravariant

instance Wrapped (a -> Bool) (a' -> Bool) (Predicate a) (Predicate a') where
  wrapped = iso Predicate getPredicate
  {-# INLINE wrapped #-}

instance Wrapped (a -> a -> Ordering) (a' -> a' -> Ordering) (Comparison a) (Comparison a') where
  wrapped = iso Comparison getComparison
  {-# INLINE wrapped #-}

instance Wrapped (a -> a -> Bool) (a' -> a' -> Bool) (Equivalence a) (Equivalence a') where
  wrapped = iso Equivalence getEquivalence
  {-# INLINE wrapped #-}

instance Wrapped (b -> a) (b' -> a') (Op a b) (Op a' b') where
  wrapped = iso Op getOp
  {-# INLINE wrapped #-}

instance Wrapped (f (g a)) (f' (g' a')) (Contravariant.Compose f g a) (Contravariant.Compose f' g' a') where
  wrapped = iso Contravariant.Compose Contravariant.getCompose
  {-# INLINE wrapped #-}

instance Wrapped (f (g a)) (f' (g' a')) (Contravariant.ComposeFC f g a) (Contravariant.ComposeFC f' g' a') where
  wrapped = iso Contravariant.ComposeFC Contravariant.getComposeFC
  {-# INLINE wrapped #-}

instance Wrapped (f (g a)) (f' (g' a')) (Contravariant.ComposeCF f g a) (Contravariant.ComposeFC f' g' a') where
  wrapped = iso Contravariant.ComposeCF Contravariant.getComposeFC
  {-# INLINE wrapped #-}

-- * tagged

instance Wrapped a b (Tagged s a) (Tagged t b) where
  wrapped = iso Tagged unTagged
  {-# INLINE wrapped #-}

-- * Control.Exception

instance Wrapped String String AssertionFailed AssertionFailed where
  wrapped = iso AssertionFailed failedAssertion
  {-# INLINE wrapped #-}

instance Wrapped String String NoMethodError NoMethodError where
  wrapped = iso NoMethodError getNoMethodError
  {-# INLINE wrapped #-}

instance Wrapped String String PatternMatchFail PatternMatchFail where
  wrapped = iso PatternMatchFail getPatternMatchFail
  {-# INLINE wrapped #-}

instance Wrapped String String RecConError RecConError where
  wrapped = iso RecConError getRecConError
  {-# INLINE wrapped #-}

instance Wrapped String String RecSelError RecSelError where
  wrapped = iso RecSelError getRecSelError
  {-# INLINE wrapped #-}

instance Wrapped String String RecUpdError RecUpdError where
  wrapped = iso RecUpdError getRecUpdError
  {-# INLINE wrapped #-}

instance Wrapped String String ErrorCall ErrorCall where
  wrapped = iso ErrorCall getErrorCall
  {-# INLINE wrapped #-}

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

getArrowMonad :: ArrowApply m  => ArrowMonad m a -> m () a
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
op :: Wrapped s s a a => (s -> a) -> a -> s
op f = review (wrapping f)
{-# INLINE op #-}

-- | This is a convenient alias for @'from' 'wrapped'@.
--
-- >>> Const "hello" & unwrapped %~ length & getConst
-- 5
unwrapped :: Wrapped t s b a => Iso a b s t
unwrapped = from wrapped
{-# INLINE unwrapped #-}

-- | A convenient type-restricted version of 'wrapped' for aiding type inference.
wrapped' :: Wrapped s s a a => Iso' s a
wrapped' = wrapped
{-# INLINE wrapped' #-}

-- | A convenient type-restricted version of 'unwrapped' for aiding type inference.
unwrapped' :: Wrapped s s a a => Iso' a s
unwrapped' = unwrapped
{-# INLINE unwrapped' #-}

-- | This is a convenient version of 'wrapped' with an argument that's ignored.
--
-- The argument is used to specify which newtype the user intends to wrap
-- by using the constructor for that newtype.
--
-- The user supplied function is /ignored/, merely its type is used.
wrapping :: Wrapped s s a a => (s -> a) -> Iso s s a a
wrapping _ = wrapped
{-# INLINE wrapping #-}

-- | This is a convenient version of 'unwrapped' with an argument that's ignored.
--
-- The argument is used to specify which newtype the user intends to /remove/
-- by using the constructor for that newtype.
--
-- The user supplied function is /ignored/, merely its type is used.
unwrapping :: Wrapped s s a a => (s -> a) -> Iso a a s s
unwrapping _ = unwrapped
{-# INLINE unwrapping #-}

-- | This is a convenient version of 'wrapped' with two arguments that are ignored.
--
-- These arguments are used to which newtype the user intends to wrap and
-- should both be the same constructor.  This redundancy is necessary
-- in order to find the full polymorphic isomorphism family.
--
-- The user supplied functions are /ignored/, merely their types are used.
wrappings :: Wrapped s t a b => (s -> a) -> (t -> b) -> Iso s t a b
wrappings _ _ = wrapped
{-# INLINE wrappings #-}

-- | This is a convenient version of 'unwrapped' with two arguments that are ignored.
--
-- These arguments are used to which newtype the user intends to remove and
-- should both be the same constructor. This redundancy is necessary
-- in order to find the full polymorphic isomorphism family.
--
-- The user supplied functions are /ignored/, merely their types are used.
unwrappings :: Wrapped t s b a => (s -> a) -> (t -> b) -> Iso a b s t
unwrappings _ _ = unwrapped
{-# INLINE unwrappings #-}

-- | This combinator is based on @ala@ from Conor McBride's work on Epigram.
--
-- As with 'wrapping', the user supplied function for the newtype is /ignored/.
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
ala :: Wrapped s s a a => (s -> a) -> ((s -> a) -> e -> a) -> e -> s
ala = au . wrapping
{-# INLINE ala #-}

-- | This combinator is based on @ala'@ from Conor McBride's work on Epigram.
--
-- As with 'wrapping', the user supplied function for the newtype is /ignored/.
--
-- >>> alaf Sum foldMap length ["hello","world"]
-- 10
alaf :: Wrapped s s a a => (s -> a) -> ((r -> a) -> e -> a) -> (r -> s) -> e -> s
alaf = auf . wrapping
{-# INLINE alaf #-}
