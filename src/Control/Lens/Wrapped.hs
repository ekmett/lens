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
  , ala, alaf
  ) where

import           Control.Applicative
import           Control.Applicative.Backwards
import           Control.Applicative.Lift
import           Control.Arrow
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
import           Data.Functor.Identity
import qualified Data.Functor.Product              as F
import           Data.Functor.Reverse
import           Data.Monoid

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Foldable

-- | 'Wrapped' provides isomorphisms to wrap and unwrap newtypes.
class Wrapped s t a b | a -> s, b -> t, a t -> s, b s -> t where
  wrapped :: Iso s t a b
  unwrapped :: Iso a b s t

instance Wrapped Bool Bool All All where
  wrapped   = iso All getAll
  unwrapped = iso getAll All

instance Wrapped Bool Bool Any Any where
  wrapped   = iso Any getAny
  unwrapped = iso getAny Any

instance Wrapped a b (Sum a) (Sum b) where
  wrapped   = isos Sum getSum Sum getSum
  unwrapped = isos getSum Sum getSum Sum

instance Wrapped a b (Product a) (Product b) where
  wrapped   = isos Product getProduct Product getProduct
  unwrapped = isos getProduct Product getProduct Product

instance Wrapped (a -> m b) (u -> n v) (Kleisli m a b) (Kleisli n u v) where
  wrapped   = isos Kleisli runKleisli Kleisli runKleisli
  unwrapped = isos runKleisli Kleisli runKleisli Kleisli

instance Wrapped (m a) (n b) (WrappedMonad m a) (WrappedMonad n b) where
  wrapped   = isos WrapMonad unwrapMonad WrapMonad unwrapMonad
  unwrapped = isos unwrapMonad WrapMonad unwrapMonad WrapMonad

instance Wrapped (a b c) (u v w) (WrappedArrow a b c) (WrappedArrow u v w) where
  wrapped   = isos WrapArrow unwrapArrow WrapArrow unwrapArrow
  unwrapped = isos unwrapArrow WrapArrow unwrapArrow WrapArrow

instance Wrapped [a] [b] (ZipList a) (ZipList b) where
  wrapped   = isos ZipList getZipList ZipList getZipList
  unwrapped = isos getZipList ZipList getZipList ZipList

instance Wrapped a b (Const a x) (Const b y) where
  wrapped   = isos Const getConst Const getConst
  unwrapped = isos getConst Const getConst Const

instance Wrapped (a -> a) (b -> b) (Endo a) (Endo b) where
  wrapped   = isos Endo appEndo Endo appEndo
  unwrapped = isos appEndo Endo appEndo Endo

instance Wrapped (Maybe a) (Maybe b) (First a) (First b) where
  wrapped   = isos First getFirst First getFirst
  unwrapped = isos getFirst First getFirst First

instance Wrapped (Maybe a) (Maybe b) (Last a) (Last b) where
  wrapped   = isos Last getLast Last getLast
  unwrapped = isos getLast Last getLast Last

instance (ArrowApply m, ArrowApply n) => Wrapped (m () a) (n () b) (ArrowMonad m a) (ArrowMonad n b) where
  wrapped   = isos ArrowMonad getArrowMonad ArrowMonad getArrowMonad
  unwrapped = isos getArrowMonad ArrowMonad getArrowMonad ArrowMonad

-- transformers

instance Wrapped (f a) (f' a') (Backwards f a) (Backwards f' a') where
  wrapped   = isos Backwards forwards Backwards forwards
  unwrapped = isos forwards Backwards forwards Backwards

instance Wrapped (f (g a)) (f' (g' a')) (Compose f g a) (Compose f' g' a') where
  wrapped   = isos Compose getCompose Compose getCompose
  unwrapped = isos getCompose Compose getCompose Compose

instance Wrapped a a' (Constant a b) (Constant a' b') where
  wrapped   = isos Constant getConstant Constant getConstant
  unwrapped = isos getConstant Constant getConstant Constant

instance Wrapped ((a -> m r) -> m r) ((a' -> m' r') -> m' r') (ContT r m a) (ContT r' m' a') where
  wrapped   = isos ContT runContT ContT runContT
  unwrapped = isos runContT ContT runContT ContT

instance Wrapped (m (Either e a)) (m' (Either e' a')) (ErrorT e m a) (ErrorT e' m' a') where
  wrapped   = isos ErrorT runErrorT ErrorT runErrorT
  unwrapped = isos runErrorT ErrorT runErrorT ErrorT

instance Wrapped a a' (Identity a) (Identity a') where
  wrapped   = isos Identity runIdentity Identity runIdentity
  unwrapped = isos runIdentity Identity runIdentity Identity

instance Wrapped (m a) (m' a') (IdentityT m a) (IdentityT m' a') where
  wrapped   = isos IdentityT runIdentityT IdentityT runIdentityT
  unwrapped = isos runIdentityT IdentityT runIdentityT IdentityT

instance (Applicative f, Applicative g) => Wrapped (f a) (g b) (Lift f a) (Lift g b) where
  wrapped   = isos Other unLift Other unLift
  unwrapped = isos unLift Other unLift Other

instance Wrapped (m [a]) (m' [a']) (ListT m a) (ListT m' a') where
  wrapped   = isos ListT runListT ListT runListT
  unwrapped = isos runListT ListT runListT ListT

instance Wrapped (m (Maybe a)) (m' (Maybe a')) (MaybeT m a) (MaybeT m' a') where
  wrapped   = isos MaybeT runMaybeT MaybeT runMaybeT
  unwrapped = isos runMaybeT MaybeT runMaybeT MaybeT

instance Wrapped (f a, g a) (f' a', g' a') (F.Product f g a) (F.Product f' g' a') where
  wrapped   = isos pair getPair pair getPair
  unwrapped = isos getPair pair getPair pair

instance Wrapped (r -> m a) (r' -> m' a') (ReaderT r m a) (ReaderT r' m' a') where
  wrapped   = isos ReaderT runReaderT ReaderT runReaderT
  unwrapped = isos runReaderT ReaderT runReaderT ReaderT

instance Wrapped (f a) (f' a') (Reverse f a) (Reverse f' a') where
  wrapped   = isos Reverse getReverse Reverse getReverse
  unwrapped = isos getReverse Reverse getReverse Reverse

instance Wrapped (r -> s -> m (a, s, w)) (r' -> s' -> m' (a', s', w')) (Lazy.RWST r w s m a) (Lazy.RWST r' w' s' m' a') where
  wrapped   = isos Lazy.RWST Lazy.runRWST Lazy.RWST Lazy.runRWST
  unwrapped = isos Lazy.runRWST Lazy.RWST Lazy.runRWST Lazy.RWST

instance Wrapped (r -> s -> m (a, s, w)) (r' -> s' -> m' (a', s', w')) (Strict.RWST r w s m a) (Strict.RWST r' w' s' m' a') where
  wrapped   = isos Strict.RWST Strict.runRWST Strict.RWST Strict.runRWST
  unwrapped = isos Strict.runRWST Strict.RWST Strict.runRWST Strict.RWST

instance Wrapped (s -> m (a, s)) (s' -> m' (a', s')) (Lazy.StateT s m a) (Lazy.StateT s' m' a') where
  wrapped   = isos Lazy.StateT Lazy.runStateT Lazy.StateT Lazy.runStateT
  unwrapped = isos Lazy.runStateT Lazy.StateT Lazy.runStateT Lazy.StateT

instance Wrapped (s -> m (a, s)) (s' -> m' (a', s')) (Strict.StateT s m a) (Strict.StateT s' m' a') where
  wrapped   = isos Strict.StateT Strict.runStateT Strict.StateT Strict.runStateT
  unwrapped = isos Strict.runStateT Strict.StateT Strict.runStateT Strict.StateT

instance Wrapped (m (a, w)) (m' (a', w')) (Lazy.WriterT w m a) (Lazy.WriterT w' m' a') where
  wrapped   = isos Lazy.WriterT Lazy.runWriterT Lazy.WriterT Lazy.runWriterT
  unwrapped = isos Lazy.runWriterT Lazy.WriterT Lazy.runWriterT Lazy.WriterT

instance Wrapped (m (a, w)) (m' (a', w')) (Strict.WriterT w m a) (Strict.WriterT w' m' a') where
  wrapped   = isos Strict.WriterT Strict.runWriterT Strict.WriterT Strict.runWriterT
  unwrapped = isos Strict.runWriterT Strict.WriterT Strict.runWriterT Strict.WriterT

getArrowMonad :: ArrowApply m  => ArrowMonad m a -> m () a
getArrowMonad (ArrowMonad x) = x

pair :: (f a, g a) -> F.Product f g a
pair = uncurry F.Pair

getPair :: F.Product f g a -> (f a, g a)
getPair (F.Pair f g) = (f, g)

-- | This is a convenient version of 'wrapped' with an argument that's ignored.
--
-- The argument is used to specify which newtype the user intends to wrap
-- by using the constructor for that newtype.
--
-- The user supplied function is /ignored/, merely its type is used.
wrapping :: Wrapped s s a a => (s -> a) -> Iso s s a a
wrapping _ = wrapped

-- | Convenient variant for 'unwrapped' with an argument that's ignored.
--
-- The argument is used to specify which newtype the user intends to remove
-- by using the constructor for that newtype.
--
-- The user supplied function is /ignored/, merely its type is used.
unwrapping :: Wrapped s s a a => (s -> a) -> Iso a a s s
unwrapping _ = unwrapped

-- | Convenience for 'wrapped' with two arguments that are ignored.
--
-- These arguments are used to which newtype the user intends to wrap and
-- should both be the same constructor.  This redundency is necessary
-- in order to find the full polymorphic isomorphism family.
--
-- The user supplied functions are /ignored/, merely their types are used.
wrappings :: Wrapped s t a b => (s -> a) -> (t -> b) -> Iso s t a b
wrappings _ _ = wrapped

-- | Convenience for 'wrapped' with two arguments that are ignored.
--
-- These arguments are used to which newtype the user intends to remove and
-- should both be the same constructor. This redundency is necessary
-- in order to find the full polymorphic isomorphism family.
--
-- The user supplied functions are /ignored/, merely their types are used.
unwrappings :: Wrapped s t a b => (s -> a) -> (t -> b) -> Iso a b s t
unwrappings _ _ = unwrapped

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
