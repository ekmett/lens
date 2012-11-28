{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Wrapped
-- Copyright   :  (C) 2012 Edward Kmett, Michael Sloan
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2, MPTCs, fundeps
--
-- The 'Wrapped' class provides similar functionality as @Control.Newtype@,
-- from the @newtype@ package, but in a more convenient and efficient form.
--
----------------------------------------------------------------------------
module Control.Lens.Wrapped
  ( Wrapped(..)
  , wrapping, unwrapping
  , wrappings, unwrappings
  , op
  , ala, alaf
  ) where

import           Control.Applicative
import           Control.Applicative.Backwards
import           Control.Applicative.Lift
import           Control.Arrow
-- import        Control.Comonad.Trans.Env
-- import        Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Traced
import           Control.Exception
import           Control.Lens.Projection
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
import           Data.Functor.Compose
import           Data.Functor.Constant
import           Data.Functor.Coproduct
import           Data.Functor.Identity
import           Data.Functor.Reverse
import           Data.Monoid

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Foldable

-- | 'Wrapped' provides isomorphisms to wrap and unwrap newtypes or
-- data types with one constructor.
class Wrapped s t a b | a -> s, b -> t, a t -> s, b s -> t where
  -- | An isomorphism between s and @a@ and a related one between @t@ and @b@, such that when @a = b@, @s = t@.
  --
  -- This is often used via 'wrapping' to aid type inference.
  wrapped   :: Iso s t a b

instance Wrapped Bool Bool All All where
  wrapped   = iso All getAll
  {-# INLINE wrapped #-}

instance Wrapped Bool Bool Any Any where
  wrapped   = iso Any getAny
  {-# INLINE wrapped #-}

instance Wrapped a b (Sum a) (Sum b) where
  wrapped   = isos Sum getSum Sum getSum
  {-# INLINE wrapped #-}

instance Wrapped a b (Product a) (Product b) where
  wrapped   = isos Product getProduct Product getProduct
  {-# INLINE wrapped #-}

instance Wrapped (a -> m b) (u -> n v) (Kleisli m a b) (Kleisli n u v) where
  wrapped   = isos Kleisli runKleisli Kleisli runKleisli
  {-# INLINE wrapped #-}

instance Wrapped (m a) (n b) (WrappedMonad m a) (WrappedMonad n b) where
  wrapped   = isos WrapMonad unwrapMonad WrapMonad unwrapMonad
  {-# INLINE wrapped #-}

instance Wrapped (a b c) (u v w) (WrappedArrow a b c) (WrappedArrow u v w) where
  wrapped   = isos WrapArrow unwrapArrow WrapArrow unwrapArrow
  {-# INLINE wrapped #-}

instance Wrapped [a] [b] (ZipList a) (ZipList b) where
  wrapped   = isos ZipList getZipList ZipList getZipList
  {-# INLINE wrapped #-}

instance Wrapped a b (Const a x) (Const b y) where
  wrapped   = isos Const getConst Const getConst
  {-# INLINE wrapped #-}

instance Wrapped (a -> a) (b -> b) (Endo a) (Endo b) where
  wrapped   = isos Endo appEndo Endo appEndo
  {-# INLINE wrapped #-}

instance Wrapped (Maybe a) (Maybe b) (First a) (First b) where
  wrapped   = isos First getFirst First getFirst
  {-# INLINE wrapped #-}

instance Wrapped (Maybe a) (Maybe b) (Last a) (Last b) where
  wrapped   = isos Last getLast Last getLast
  {-# INLINE wrapped #-}

instance (ArrowApply m, ArrowApply n) => Wrapped (m () a) (n () b) (ArrowMonad m a) (ArrowMonad n b) where
  wrapped   = isos ArrowMonad getArrowMonad ArrowMonad getArrowMonad
  {-# INLINE wrapped #-}

-- transformers

instance Wrapped (f a) (f' a') (Backwards f a) (Backwards f' a') where
  wrapped   = isos Backwards forwards Backwards forwards
  {-# INLINE wrapped #-}

instance Wrapped (f (g a)) (f' (g' a')) (Compose f g a) (Compose f' g' a') where
  wrapped   = isos Compose getCompose Compose getCompose
  {-# INLINE wrapped #-}

instance Wrapped a a' (Constant a b) (Constant a' b') where
  wrapped   = isos Constant getConstant Constant getConstant
  {-# INLINE wrapped #-}

instance Wrapped ((a -> m r) -> m r) ((a' -> m' r') -> m' r') (ContT r m a) (ContT r' m' a') where
  wrapped   = isos ContT runContT ContT runContT
  {-# INLINE wrapped #-}

instance Wrapped (m (Either e a)) (m' (Either e' a')) (ErrorT e m a) (ErrorT e' m' a') where
  wrapped   = isos ErrorT runErrorT ErrorT runErrorT
  {-# INLINE wrapped #-}

instance Wrapped a a' (Identity a) (Identity a') where
  wrapped   = isos Identity runIdentity Identity runIdentity
  {-# INLINE wrapped #-}

instance Wrapped (m a) (m' a') (IdentityT m a) (IdentityT m' a') where
  wrapped   = isos IdentityT runIdentityT IdentityT runIdentityT
  {-# INLINE wrapped #-}

instance (Applicative f, Applicative g) => Wrapped (f a) (g b) (Lift f a) (Lift g b) where
  wrapped   = isos Other unLift Other unLift
  {-# INLINE wrapped #-}

instance Wrapped (m [a]) (m' [a']) (ListT m a) (ListT m' a') where
  wrapped   = isos ListT runListT ListT runListT
  {-# INLINE wrapped #-}

instance Wrapped (m (Maybe a)) (m' (Maybe a')) (MaybeT m a) (MaybeT m' a') where
  wrapped   = isos MaybeT runMaybeT MaybeT runMaybeT
  {-# INLINE wrapped #-}

instance Wrapped (r -> m a) (r' -> m' a') (ReaderT r m a) (ReaderT r' m' a') where
  wrapped   = isos ReaderT runReaderT ReaderT runReaderT
  {-# INLINE wrapped #-}

instance Wrapped (f a) (f' a') (Reverse f a) (Reverse f' a') where
  wrapped   = isos Reverse getReverse Reverse getReverse
  {-# INLINE wrapped #-}

instance Wrapped (r -> s -> m (a, s, w)) (r' -> s' -> m' (a', s', w')) (Lazy.RWST r w s m a) (Lazy.RWST r' w' s' m' a') where
  wrapped   = isos Lazy.RWST Lazy.runRWST Lazy.RWST Lazy.runRWST
  {-# INLINE wrapped #-}

instance Wrapped (r -> s -> m (a, s, w)) (r' -> s' -> m' (a', s', w')) (Strict.RWST r w s m a) (Strict.RWST r' w' s' m' a') where
  wrapped   = isos Strict.RWST Strict.runRWST Strict.RWST Strict.runRWST
  {-# INLINE wrapped #-}

instance Wrapped (s -> m (a, s)) (s' -> m' (a', s')) (Lazy.StateT s m a) (Lazy.StateT s' m' a') where
  wrapped   = isos Lazy.StateT Lazy.runStateT Lazy.StateT Lazy.runStateT
  {-# INLINE wrapped #-}

instance Wrapped (s -> m (a, s)) (s' -> m' (a', s')) (Strict.StateT s m a) (Strict.StateT s' m' a') where
  wrapped   = isos Strict.StateT Strict.runStateT Strict.StateT Strict.runStateT
  {-# INLINE wrapped #-}

instance Wrapped (m (a, w)) (m' (a', w')) (Lazy.WriterT w m a) (Lazy.WriterT w' m' a') where
  wrapped   = isos Lazy.WriterT Lazy.runWriterT Lazy.WriterT Lazy.runWriterT
  {-# INLINE wrapped #-}

instance Wrapped (m (a, w)) (m' (a', w')) (Strict.WriterT w m a) (Strict.WriterT w' m' a') where
  wrapped   = isos Strict.WriterT Strict.runWriterT Strict.WriterT Strict.runWriterT
  {-# INLINE wrapped #-}

-- comonad-transformers

instance Wrapped (Either (f a) (g a)) (Either (f' a') (g' a')) (Coproduct f g a) (Coproduct f' g' a') where
  wrapped   = isos Coproduct getCoproduct Coproduct getCoproduct
  {-# INLINE wrapped #-}

instance Wrapped (w (m -> a)) (w' (m' -> a')) (TracedT m w a) (TracedT m' w' a') where
  wrapped   = isos TracedT runTracedT TracedT runTracedT
  {-# INLINE wrapped #-}

-- exceptions

instance Wrapped String String AssertionFailed AssertionFailed where
  wrapped   = isos AssertionFailed failedAssertion AssertionFailed failedAssertion
  {-# INLINE wrapped #-}

instance Wrapped String String NoMethodError NoMethodError where
  wrapped   = isos NoMethodError getNoMethodError NoMethodError getNoMethodError
  {-# INLINE wrapped #-}

instance Wrapped String String PatternMatchFail PatternMatchFail where
  wrapped   = isos PatternMatchFail getPatternMatchFail PatternMatchFail getPatternMatchFail
  {-# INLINE wrapped #-}

instance Wrapped String String RecConError RecConError where
  wrapped   = isos RecConError getRecConError RecConError getRecConError
  {-# INLINE wrapped #-}

instance Wrapped String String RecSelError RecSelError where
  wrapped   = isos RecSelError getRecSelError RecSelError getRecSelError
  {-# INLINE wrapped #-}

instance Wrapped String String RecUpdError RecUpdError where
  wrapped   = isos RecUpdError getRecUpdError RecUpdError getRecUpdError
  {-# INLINE wrapped #-}

instance Wrapped String String ErrorCall ErrorCall where
  wrapped   = isos ErrorCall getErrorCall ErrorCall getErrorCall
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

-- | Given the constructor for a @Wrapped@ type, return a
-- deconstructor that is its inverse.
--
-- Assuming the @Wrapped@ instance is legal, these laws hold:
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

-- | This is a convenient alias for @'from' 'wrapped'@
--
-- >>> Const "hello" & unwrapped %~ length & getConst
-- 5
unwrapped :: Wrapped s t a b => Iso a b s t
unwrapped = from wrapped
{-# INLINE unwrapped #-}

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
-- should both be the same constructor.  This redundency is necessary
-- in order to find the full polymorphic isomorphism family.
--
-- The user supplied functions are /ignored/, merely their types are used.
wrappings :: Wrapped s t a b => (s -> a) -> (t -> b) -> Iso s t a b
wrappings _ _ = wrapped
{-# INLINE wrappings #-}

-- | This is a convenient version of 'unwrapped' with two arguments that are ignored.
--
-- These arguments are used to which newtype the user intends to remove and
-- should both be the same constructor. This redundency is necessary
-- in order to find the full polymorphic isomorphism family.
--
-- The user supplied functions are /ignored/, merely their types are used.
unwrappings :: Wrapped s t a b => (s -> a) -> (t -> b) -> Iso a b s t
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

-- |
-- This combinator is based on @ala'@ from Conor McBride's work on Epigram.
--
-- As with 'wrapping', the user supplied function for the newtype is /ignored/.
--
-- >>> alaf Sum foldMap length ["hello","world"]
-- 10
alaf :: Wrapped s s a a => (s -> a) -> ((r -> a) -> e -> a) -> (r -> s) -> e -> s
alaf = auf . wrapping
{-# INLINE alaf #-}
