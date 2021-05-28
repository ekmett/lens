{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Wrapped
-- Copyright   :  (C) 2012-16 Edward Kmett, Michael Sloan
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
  -- * Pattern Synonyms
  , pattern Wrapped
  , pattern Unwrapped
  -- * Generics
  , _GWrapped'
  ) where

#include "HsBaseConfig.h"

import qualified Control.Alternative.Free as Free
import qualified Control.Applicative as Applicative
import           Control.Applicative hiding (WrappedArrow(..))
import           Control.Applicative.Trans.Free
import           Control.Arrow
import           Control.Applicative.Backwards
import           Control.Comonad.Trans.Cofree
import           Control.Comonad.Trans.Coiter
import           Control.Comonad.Trans.Traced
import           Control.Exception
import           Control.Lens.Getter
import           Control.Lens.Internal.CTypes
import           Control.Lens.Iso
import           Control.Lens.Review
import           Control.Monad.Catch.Pure
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Iter
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy      as Lazy
import qualified Control.Monad.Trans.RWS.Strict    as Strict
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Data.Bifunctor.Biff
import           Data.Bifunctor.Clown
import           Data.Bifunctor.Fix
import           Data.Bifunctor.Flip
import           Data.Bifunctor.Join
import           Data.Bifunctor.Joker
import           Data.Bifunctor.Tannen
import           Data.Bifunctor.Wrapped
import           Data.Foldable as Foldable
import           Data.Functor.Bind
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import qualified Data.Functor.Contravariant.Compose as Contravariant
import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Functor.Reverse
import           Data.Hashable
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy (HashMap)
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Monoid as Monoid
import           Data.Monoid
import qualified Data.Profunctor as Profunctor
import           Data.Profunctor hiding (WrappedArrow(..))
import           Data.Profunctor.Cayley
import qualified Data.Semigroup as S
import           Data.Semigroupoid
import qualified Data.Semigroupoid.Dual as Semigroupoid
import           Data.Semigroupoid.Static
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Tagged
import qualified Data.Vector as Vector
import qualified Data.Vector.Primitive as Prim
import           Data.Vector.Primitive (Prim)
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Storable as Storable
import           Foreign.C.Error
import           Foreign.C.Types
import           Foreign.Storable (Storable)
import qualified GHC.Generics as Generic
import           GHC.Generics hiding (from, to)
import           System.Posix.Types
import           Data.Ord (Down(Down))

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Foldable (foldMap)
-- >>> import Data.Monoid (Sum (..), Product (..), All (..), Any (..))

-- | 'Wrapped' provides isomorphisms to wrap and unwrap newtypes or
-- data types with one constructor.
class Wrapped s where
  type Unwrapped s :: Type
  type Unwrapped s = GUnwrapped (Rep s)

  -- | An isomorphism between @s@ and @a@.
  --
  -- If your type has a 'Generic' instance, '_Wrapped'' will default to '_GWrapped'',
  -- and you can choose to not override it with your own definition.
  _Wrapped' :: Iso' s (Unwrapped s)
  default _Wrapped' :: (Generic s, D1 d (C1 c (S1 s' (Rec0 a))) ~ Rep s, Unwrapped s ~ GUnwrapped (Rep s))
                    => Iso' s (Unwrapped s)
  _Wrapped' = _GWrapped'
  {-# INLINE _Wrapped' #-}

-- | Implement the '_Wrapped' operation for a type using its 'Generic' instance.
_GWrapped' :: (Generic s, D1 d (C1 c (S1 s' (Rec0 a))) ~ Rep s, Unwrapped s ~ GUnwrapped (Rep s))
           => Iso' s (Unwrapped s)
_GWrapped' = iso Generic.from Generic.to . iso remitter reviewer
  where
    remitter (M1 (M1 (M1 (K1 x)))) = x
    reviewer x = M1 (M1 (M1 (K1 x)))
{-# INLINE _GWrapped' #-}

type family GUnwrapped (rep :: Type -> Type) :: Type
type instance GUnwrapped (D1 d (C1 c (S1 s (Rec0 a)))) = a

pattern Wrapped :: Rewrapped s s => Unwrapped s -> s
pattern Wrapped a <- (view _Wrapped -> a) where
  Wrapped a = review _Wrapped a

pattern Unwrapped :: Rewrapped t t => t -> Unwrapped t
pattern Unwrapped a <- (view _Unwrapped -> a) where
  Unwrapped a = review _Unwrapped a

-- This can be used to help inference between the wrappers
class Wrapped s => Rewrapped (s :: Type) (t :: Type)

class    (Rewrapped s t, Rewrapped t s) => Rewrapping s t
instance (Rewrapped s t, Rewrapped t s) => Rewrapping s t

_Unwrapped' :: Wrapped s => Iso' (Unwrapped s) s
_Unwrapped' = from _Wrapped'
{-# INLINE _Unwrapped' #-}

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

instance (t ~ Applicative.WrappedArrow a' b' c') => Rewrapped (Applicative.WrappedArrow a b c) t
instance Wrapped (Applicative.WrappedArrow a b c) where
  type Unwrapped (Applicative.WrappedArrow a b c) = a b c
  _Wrapped' = iso Applicative.unwrapArrow Applicative.WrapArrow
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

instance (t ~ Endo b) => Rewrapped (Endo a) t
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

instance (t ~ Monoid.Alt g b) => Rewrapped (Monoid.Alt f a) t
instance Wrapped (Monoid.Alt f a) where
  type Unwrapped (Monoid.Alt f a) = f a
  _Wrapped' = iso Monoid.getAlt Monoid.Alt
  {-# INLINE _Wrapped' #-}

#if MIN_VERSION_base(4,12,0)
instance (t ~ Monoid.Ap g b) => Rewrapped (Monoid.Ap f a) t
instance Wrapped (Monoid.Ap f a) where
  type Unwrapped (Monoid.Ap f a) = f a
  _Wrapped' = iso Monoid.getAp Monoid.Ap
  {-# INLINE _Wrapped' #-}
#endif

instance t ~ ArrowMonad m' a' => Rewrapped (ArrowMonad m a) t
instance Wrapped (ArrowMonad m a) where
  type Unwrapped (ArrowMonad m a) = m () a
  _Wrapped' = iso getArrowMonad ArrowMonad
  {-# INLINE _Wrapped' #-}

instance t ~ Down b => Rewrapped (Down a) t
instance Wrapped (Down a) where
  type Unwrapped (Down a) = a
  _Wrapped' = iso (\(Down a) -> a) Down
  {-# INLINE _Wrapped' #-}

instance Rewrapped Errno t
instance Wrapped Errno where
  type Unwrapped Errno = CInt
  _Wrapped' = iso (\(Errno x) -> x) Errno
  {-# INLINE _Wrapped' #-}

getArrowMonad :: ArrowMonad m a -> m () a
getArrowMonad (ArrowMonad x) = x
{-# INLINE getArrowMonad #-}

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

instance (t ~ ExceptT e' m' a') => Rewrapped (ExceptT e m a) t
instance Wrapped (ExceptT e m a) where
  type Unwrapped (ExceptT e m a) = m (Either e a)
  _Wrapped' = iso runExceptT ExceptT
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

instance (t ~ ReaderT s n b) => Rewrapped (ReaderT r m a) t
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

-- * bifunctors

instance (t ~ Biff p' f' g' a' b') => Rewrapped (Biff p f g a b) t
instance Wrapped (Biff p f g a b) where
  type Unwrapped (Biff p f g a b) = p (f a) (g b)
  _Wrapped' = iso runBiff Biff
  {-# INLINE _Wrapped' #-}

instance (t ~ Clown f' a' b') => Rewrapped (Clown f a b) t
instance Wrapped (Clown f a b) where
  type Unwrapped (Clown f a b) = f a
  _Wrapped' = iso runClown Clown
  {-# INLINE _Wrapped' #-}

instance (t ~ Fix p' a') => Rewrapped (Fix p a) t
instance Wrapped (Fix p a) where
  type Unwrapped (Fix p a) = p (Fix p a) a
  _Wrapped' = iso out In
  {-# INLINE _Wrapped' #-}

instance (t ~ Flip p' a' b') => Rewrapped (Flip p a b) t
instance Wrapped (Flip p a b) where
  type Unwrapped (Flip p a b) = p b a
  _Wrapped' = iso runFlip Flip
  {-# INLINE _Wrapped' #-}

instance (t ~ Join p' a') => Rewrapped (Join p a) t
instance Wrapped (Join p a) where
  type Unwrapped (Join p a) = p a a
  _Wrapped' = iso runJoin Join
  {-# INLINE _Wrapped' #-}

instance (t ~ Joker g' a' b') => Rewrapped (Joker g a b) t
instance Wrapped (Joker g a b) where
  type Unwrapped (Joker g a b) = g b
  _Wrapped' = iso runJoker Joker
  {-# INLINE _Wrapped' #-}

instance (t ~ Tannen f' p' a' b') => Rewrapped (Tannen f p a b) t
instance Wrapped (Tannen f p a b) where
  type Unwrapped (Tannen f p a b) = f (p a b)
  _Wrapped' = iso runTannen Tannen
  {-# INLINE _Wrapped' #-}

instance (t ~ WrappedBifunctor p' a' b') => Rewrapped (WrappedBifunctor p a b) t
instance Wrapped (WrappedBifunctor p a b) where
  type Unwrapped (WrappedBifunctor p a b) = p a b
  _Wrapped' = iso unwrapBifunctor WrapBifunctor
  {-# INLINE _Wrapped' #-}

-- * comonad

instance (t ~ TracedT m' w' a') => Rewrapped (TracedT m w a) t
instance Wrapped (TracedT m w a) where
  type Unwrapped (TracedT m w a) = w (m -> a)
  _Wrapped' = iso runTracedT TracedT
  {-# INLINE _Wrapped' #-}

-- * exceptions

instance (t ~ CatchT m' a') => Rewrapped (CatchT m a) t
instance Wrapped (CatchT m a) where
  type Unwrapped (CatchT m a) = m (Either SomeException a)
  _Wrapped' = iso runCatchT CatchT
  {-# INLINE _Wrapped' #-}

-- * free

instance (t ~ Free.Alt f' a') => Rewrapped (Free.Alt f a) t
instance Wrapped (Free.Alt f a) where
  type Unwrapped (Free.Alt f a) = [Free.AltF f a]
  _Wrapped' = iso Free.alternatives Free.Alt
  {-# INLINE _Wrapped' #-}

instance (t ~ ApT f' g' a') => Rewrapped (ApT f g a) t
instance Wrapped (ApT f g a) where
  type Unwrapped (ApT f g a) = g (ApF f g a)
  _Wrapped' = iso getApT ApT
  {-# INLINE _Wrapped' #-}

instance (t ~ CofreeT f' w' a') => Rewrapped (CofreeT f w a) t
instance Wrapped (CofreeT f w a) where
  type Unwrapped (CofreeT f w a) = w (CofreeF f a (CofreeT f w a))
  _Wrapped' = iso runCofreeT CofreeT
  {-# INLINE _Wrapped' #-}

instance (t ~ CoiterT w' a') => Rewrapped (CoiterT w a) t
instance Wrapped (CoiterT w a) where
  type Unwrapped (CoiterT w a) = w (a, CoiterT w a)
  _Wrapped' = iso runCoiterT CoiterT
  {-# INLINE _Wrapped' #-}

instance (t ~ FreeT f' m' a') => Rewrapped (FreeT f m a) t
instance Wrapped (FreeT f m a) where
  type Unwrapped (FreeT f m a) = m (FreeF f a (FreeT f m a))
  _Wrapped' = iso runFreeT FreeT
  {-# INLINE _Wrapped' #-}

instance (t ~ IterT m' a') => Rewrapped (IterT m a) t
instance Wrapped (IterT m a) where
  type Unwrapped (IterT m a) = m (Either a (IterT m a))
  _Wrapped' = iso runIterT IterT
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

-- * profunctors

instance (t ~ Star f' d' c') => Rewrapped (Star f d c) t
instance Wrapped (Star f d c) where
  type Unwrapped (Star f d c) = d -> f c
  _Wrapped' = iso runStar Star
  {-# INLINE _Wrapped' #-}

instance (t ~ Costar f' d' c') => Rewrapped (Costar f d c) t
instance Wrapped (Costar f d c) where
  type Unwrapped (Costar f d c) = f d -> c
  _Wrapped' = iso runCostar Costar
  {-# INLINE _Wrapped' #-}

instance (t ~ Profunctor.WrappedArrow p' a' b') => Rewrapped (Profunctor.WrappedArrow p a b) t
instance Wrapped (Profunctor.WrappedArrow p a b) where
  type Unwrapped (Profunctor.WrappedArrow p a b) = p a b
  _Wrapped' = iso Profunctor.unwrapArrow Profunctor.WrapArrow
  {-# INLINE _Wrapped' #-}

instance (t ~ Forget r' a' b') => Rewrapped (Forget r a b) t
instance Wrapped (Forget r a b) where
  type Unwrapped (Forget r a b) = a -> r
  _Wrapped' = iso runForget Forget
  {-# INLINE _Wrapped' #-}

instance (t ~ Cayley f' p' a' b') => Rewrapped (Cayley f p a b) t
instance Wrapped (Cayley f p a b) where
  type Unwrapped (Cayley f p a b) = f (p a b)
  _Wrapped' = iso runCayley Cayley
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

-- * semigroupoids

instance (t ~ WrappedApplicative f' a') => Rewrapped (WrappedApplicative f a) t
instance Wrapped (WrappedApplicative f a) where
  type Unwrapped (WrappedApplicative f a) = f a
  _Wrapped' = iso unwrapApplicative WrapApplicative
  {-# INLINE _Wrapped' #-}

instance (t ~ MaybeApply f' a') => Rewrapped (MaybeApply f a) t
instance Wrapped (MaybeApply f a) where
  type Unwrapped (MaybeApply f a) = Either (f a) a
  _Wrapped' = iso runMaybeApply MaybeApply
  {-# INLINE _Wrapped' #-}

instance (t ~ WrappedCategory k' a' b') => Rewrapped (WrappedCategory k a b) t
instance Wrapped (WrappedCategory k a b) where
  type Unwrapped (WrappedCategory k a b) = k a b
  _Wrapped' = iso unwrapCategory WrapCategory
  {-# INLINE _Wrapped' #-}

instance (t ~ Semi m' a' b') => Rewrapped (Semi m a b) t
instance Wrapped (Semi m a b) where
  type Unwrapped (Semi m a b) = m
  _Wrapped' = iso getSemi Semi
  {-# INLINE _Wrapped' #-}

instance (t ~ Semigroupoid.Dual k' a' b') => Rewrapped (Semigroupoid.Dual k a b) t
instance Wrapped (Semigroupoid.Dual k a b) where
  type Unwrapped (Semigroupoid.Dual k a b) = k b a
  _Wrapped' = iso Semigroupoid.getDual Semigroupoid.Dual
  {-# INLINE _Wrapped' #-}

instance (t ~ Static f' a' b') => Rewrapped (Static f a b) t
instance Wrapped (Static f a b) where
  type Unwrapped (Static f a b) = f (a -> b)
  _Wrapped' = iso runStatic Static
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

instance (t ~ TypeError) => Rewrapped TypeError t
instance Wrapped TypeError where
  type Unwrapped TypeError = String
  _Wrapped' = iso getTypeError TypeError
  {-# INLINE _Wrapped' #-}

getTypeError :: TypeError -> String
getTypeError (TypeError x) = x
{-# INLINE getTypeError #-}

#if MIN_VERSION_base(4,10,0)
instance (t ~ CompactionFailed) => Rewrapped CompactionFailed t
instance Wrapped CompactionFailed where
  type Unwrapped CompactionFailed = String
  _Wrapped' = iso getCompactionFailed CompactionFailed
  {-# INLINE _Wrapped' #-}

getCompactionFailed :: CompactionFailed -> String
getCompactionFailed (CompactionFailed x) = x
{-# INLINE getCompactionFailed #-}
#endif

getErrorCall :: ErrorCall -> String
getErrorCall (ErrorCallWithLocation x _) = x
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

-- * Foreign.C.Types

instance Rewrapped CChar t
instance Wrapped CChar where
  type Unwrapped CChar = HTYPE_CHAR
  _Wrapped' = iso (\(CChar x) -> x) CChar
  {-# INLINE _Wrapped' #-}

instance Rewrapped CSChar t
instance Wrapped CSChar where
  type Unwrapped CSChar = HTYPE_SIGNED_CHAR
  _Wrapped' = iso (\(CSChar x) -> x) CSChar
  {-# INLINE _Wrapped' #-}

instance Rewrapped CUChar t
instance Wrapped CUChar where
  type Unwrapped CUChar = HTYPE_UNSIGNED_CHAR
  _Wrapped' = iso (\(CUChar x) -> x) CUChar
  {-# INLINE _Wrapped' #-}

instance Rewrapped CShort t
instance Wrapped CShort where
  type Unwrapped CShort = HTYPE_SHORT
  _Wrapped' = iso (\(CShort x) -> x) CShort
  {-# INLINE _Wrapped' #-}

instance Rewrapped CUShort t
instance Wrapped CUShort where
  type Unwrapped CUShort = HTYPE_UNSIGNED_SHORT
  _Wrapped' = iso (\(CUShort x) -> x) CUShort
  {-# INLINE _Wrapped' #-}

instance Rewrapped CInt t
instance Wrapped CInt where
  type Unwrapped CInt = HTYPE_INT
  _Wrapped' = iso (\(CInt x) -> x) CInt
  {-# INLINE _Wrapped' #-}

instance Rewrapped CUInt t
instance Wrapped CUInt where
  type Unwrapped CUInt = HTYPE_UNSIGNED_INT
  _Wrapped' = iso (\(CUInt x) -> x) CUInt
  {-# INLINE _Wrapped' #-}

instance Rewrapped CLong t
instance Wrapped CLong where
  type Unwrapped CLong = HTYPE_LONG
  _Wrapped' = iso (\(CLong x) -> x) CLong
  {-# INLINE _Wrapped' #-}

instance Rewrapped CULong t
instance Wrapped CULong where
  type Unwrapped CULong = HTYPE_UNSIGNED_LONG
  _Wrapped' = iso (\(CULong x) -> x) CULong
  {-# INLINE _Wrapped' #-}

instance Rewrapped CLLong t
instance Wrapped CLLong where
  type Unwrapped CLLong = HTYPE_LONG_LONG
  _Wrapped' = iso (\(CLLong x) -> x) CLLong
  {-# INLINE _Wrapped' #-}

instance Rewrapped CULLong t
instance Wrapped CULLong where
  type Unwrapped CULLong = HTYPE_UNSIGNED_LONG_LONG
  _Wrapped' = iso (\(CULLong x) -> x) CULLong
  {-# INLINE _Wrapped' #-}

instance Rewrapped CFloat t
instance Wrapped CFloat where
  type Unwrapped CFloat = HTYPE_FLOAT
  _Wrapped' = iso (\(CFloat x) -> x) CFloat
  {-# INLINE _Wrapped' #-}

instance Rewrapped CDouble t
instance Wrapped CDouble where
  type Unwrapped CDouble = HTYPE_DOUBLE
  _Wrapped' = iso (\(CDouble x) -> x) CDouble
  {-# INLINE _Wrapped' #-}

instance Rewrapped CPtrdiff t
instance Wrapped CPtrdiff where
  type Unwrapped CPtrdiff = HTYPE_PTRDIFF_T
  _Wrapped' = iso (\(CPtrdiff x) -> x) CPtrdiff
  {-# INLINE _Wrapped' #-}

instance Rewrapped CSize t
instance Wrapped CSize where
  type Unwrapped CSize = HTYPE_SIZE_T
  _Wrapped' = iso (\(CSize x) -> x) CSize
  {-# INLINE _Wrapped' #-}

instance Rewrapped CWchar t
instance Wrapped CWchar where
  type Unwrapped CWchar = HTYPE_WCHAR_T
  _Wrapped' = iso (\(CWchar x) -> x) CWchar
  {-# INLINE _Wrapped' #-}

instance Rewrapped CSigAtomic t
instance Wrapped CSigAtomic where
  type Unwrapped CSigAtomic = HTYPE_SIG_ATOMIC_T
  _Wrapped' = iso (\(CSigAtomic x) -> x) CSigAtomic
  {-# INLINE _Wrapped' #-}

instance Rewrapped CClock t
instance Wrapped CClock where
  type Unwrapped CClock = HTYPE_CLOCK_T
  _Wrapped' = iso (\(CClock x) -> x) CClock
  {-# INLINE _Wrapped' #-}

instance Rewrapped CTime t
instance Wrapped CTime where
  type Unwrapped CTime = HTYPE_TIME_T
  _Wrapped' = iso (\(CTime x) -> x) CTime
  {-# INLINE _Wrapped' #-}

instance Rewrapped CUSeconds t
instance Wrapped CUSeconds where
  type Unwrapped CUSeconds = HTYPE_USECONDS_T
  _Wrapped' = iso (\(CUSeconds x) -> x) CUSeconds
  {-# INLINE _Wrapped' #-}

instance Rewrapped CSUSeconds t
instance Wrapped CSUSeconds where
  type Unwrapped CSUSeconds = HTYPE_SUSECONDS_T
  _Wrapped' = iso (\(CSUSeconds x) -> x) CSUSeconds
  {-# INLINE _Wrapped' #-}

instance Rewrapped CIntPtr t
instance Wrapped CIntPtr where
  type Unwrapped CIntPtr = HTYPE_INTPTR_T
  _Wrapped' = iso (\(CIntPtr x) -> x) CIntPtr
  {-# INLINE _Wrapped' #-}

instance Rewrapped CUIntPtr t
instance Wrapped CUIntPtr where
  type Unwrapped CUIntPtr = HTYPE_UINTPTR_T
  _Wrapped' = iso (\(CUIntPtr x) -> x) CUIntPtr
  {-# INLINE _Wrapped' #-}

instance Rewrapped CIntMax t
instance Wrapped CIntMax where
  type Unwrapped CIntMax = HTYPE_INTMAX_T
  _Wrapped' = iso (\(CIntMax x) -> x) CIntMax
  {-# INLINE _Wrapped' #-}

instance Rewrapped CUIntMax t
instance Wrapped CUIntMax where
  type Unwrapped CUIntMax = HTYPE_UINTMAX_T
  _Wrapped' = iso (\(CUIntMax x) -> x) CUIntMax
  {-# INLINE _Wrapped' #-}

-- * GHC.Generics

instance (t ~ Par1 p') => Rewrapped (Par1 p) t
instance Wrapped (Par1 p) where
  type Unwrapped (Par1 p) = p
  _Wrapped' = iso unPar1 Par1
  {-# INLINE _Wrapped' #-}

instance (t ~ Rec1 f' p') => Rewrapped (Rec1 f p) t
instance Wrapped (Rec1 f p) where
  type Unwrapped (Rec1 f p) = f p
  _Wrapped' = iso unRec1 Rec1
  {-# INLINE _Wrapped' #-}

instance (t ~ K1 i' c' p') => Rewrapped (K1 i c p) t
instance Wrapped (K1 i c p) where
  type Unwrapped (K1 i c p) = c
  _Wrapped' = iso unK1 K1
  {-# INLINE _Wrapped' #-}

instance (t ~ M1 i' c' f' p') => Rewrapped (M1 i c f p) t
instance Wrapped (M1 i c f p) where
  type Unwrapped (M1 i c f p) = f p
  _Wrapped' = iso unM1 M1
  {-# INLINE _Wrapped' #-}

instance (t ~ (f' :.: g') p') => Rewrapped ((f :.: g) p) t
instance Wrapped ((f :.: g) p) where
  type Unwrapped ((f :.: g) p) = f (g p)
  _Wrapped' = iso unComp1 Comp1
  {-# INLINE _Wrapped' #-}

-- * System.Posix.Types

#if defined(HTYPE_DEV_T)
instance Rewrapped CDev t
instance Wrapped CDev where
  type Unwrapped CDev = HTYPE_DEV_T
  _Wrapped' = iso (\(CDev x) -> x) CDev
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_INO_T)
instance Rewrapped CIno t
instance Wrapped CIno where
  type Unwrapped CIno = HTYPE_INO_T
  _Wrapped' = iso (\(CIno x) -> x) CIno
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_MODE_T)
instance Rewrapped CMode t
instance Wrapped CMode where
  type Unwrapped CMode = HTYPE_MODE_T
  _Wrapped' = iso (\(CMode x) -> x) CMode
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_OFF_T)
instance Rewrapped COff t
instance Wrapped COff where
  type Unwrapped COff = HTYPE_OFF_T
  _Wrapped' = iso (\(COff x) -> x) COff
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_PID_T)
instance Rewrapped CPid t
instance Wrapped CPid where
  type Unwrapped CPid = HTYPE_PID_T
  _Wrapped' = iso (\(CPid x) -> x) CPid
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_SSIZE_T)
instance Rewrapped CSsize t
instance Wrapped CSsize where
  type Unwrapped CSsize = HTYPE_SSIZE_T
  _Wrapped' = iso (\(CSsize x) -> x) CSsize
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_GID_T)
instance Rewrapped CGid t
instance Wrapped CGid where
  type Unwrapped CGid = HTYPE_GID_T
  _Wrapped' = iso (\(CGid x) -> x) CGid
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_NLINK_T)
instance Rewrapped CNlink t
instance Wrapped CNlink where
  type Unwrapped CNlink = HTYPE_NLINK_T
  _Wrapped' = iso (\(CNlink x) -> x) CNlink
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_UID_T)
instance Rewrapped CUid t
instance Wrapped CUid where
  type Unwrapped CUid = HTYPE_UID_T
  _Wrapped' = iso (\(CUid x) -> x) CUid
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_CC_T)
instance Rewrapped CCc t
instance Wrapped CCc where
  type Unwrapped CCc = HTYPE_CC_T
  _Wrapped' = iso (\(CCc x) -> x) CCc
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_SPEED_T)
instance Rewrapped CSpeed t
instance Wrapped CSpeed where
  type Unwrapped CSpeed = HTYPE_SPEED_T
  _Wrapped' = iso (\(CSpeed x) -> x) CSpeed
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_TCFLAG_T)
instance Rewrapped CTcflag t
instance Wrapped CTcflag where
  type Unwrapped CTcflag = HTYPE_TCFLAG_T
  _Wrapped' = iso (\(CTcflag x) -> x) CTcflag
  {-# INLINE _Wrapped' #-}
#endif

#if defined(HTYPE_RLIM_T)
instance Rewrapped CRLim t
instance Wrapped CRLim where
  type Unwrapped CRLim = HTYPE_RLIM_T
  _Wrapped' = iso (\(CRLim x) -> x) CRLim
  {-# INLINE _Wrapped' #-}
#endif

instance Rewrapped Fd t
instance Wrapped Fd where
  type Unwrapped Fd = CInt
  _Wrapped' = iso (\(Fd x) -> x) Fd
  {-# INLINE _Wrapped' #-}

#if MIN_VERSION_base(4,10,0)
instance Rewrapped CBool t
instance Wrapped CBool where
  type Unwrapped CBool = HTYPE_BOOL
  _Wrapped' = iso (\(CBool x) -> x) CBool
  {-# INLINE _Wrapped' #-}

# if defined(HTYPE_BLKSIZE_T)
instance Rewrapped CBlkSize t
instance Wrapped CBlkSize where
  type Unwrapped CBlkSize = HTYPE_BLKSIZE_T
  _Wrapped' = iso (\(CBlkSize x) -> x) CBlkSize
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_BLKCNT_T)
instance Rewrapped CBlkCnt t
instance Wrapped CBlkCnt where
  type Unwrapped CBlkCnt = HTYPE_BLKCNT_T
  _Wrapped' = iso (\(CBlkCnt x) -> x) CBlkCnt
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_CLOCKID_T)
instance Rewrapped CClockId t
instance Wrapped CClockId where
  type Unwrapped CClockId = HTYPE_CLOCKID_T
  _Wrapped' = iso (\(CClockId x) -> x) CClockId
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_FSBLKCNT_T)
instance Rewrapped CFsBlkCnt t
instance Wrapped CFsBlkCnt where
  type Unwrapped CFsBlkCnt = HTYPE_FSBLKCNT_T
  _Wrapped' = iso (\(CFsBlkCnt x) -> x) CFsBlkCnt
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_FSFILCNT_T)
instance Rewrapped CFsFilCnt t
instance Wrapped CFsFilCnt where
  type Unwrapped CFsFilCnt = HTYPE_FSFILCNT_T
  _Wrapped' = iso (\(CFsFilCnt x) -> x) CFsFilCnt
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_ID_T)
instance Rewrapped CId t
instance Wrapped CId where
  type Unwrapped CId = HTYPE_ID_T
  _Wrapped' = iso (\(CId x) -> x) CId
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_KEY_T)
instance Rewrapped CKey t
instance Wrapped CKey where
  type Unwrapped CKey = HTYPE_KEY_T
  _Wrapped' = iso (\(CKey x) -> x) CKey
  {-# INLINE _Wrapped' #-}
# endif

# if defined(HTYPE_TIMER_T)
instance Rewrapped CTimer t
instance Wrapped CTimer where
  type Unwrapped CTimer = HTYPE_TIMER_T
  _Wrapped' = iso (\(CTimer x) -> x) CTimer
  {-# INLINE _Wrapped' #-}
# endif
#endif

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
-- >>> ala Product foldMap [1,2,3,4]
-- 24
--
--
-- You may want to think of this combinator as having the following, simpler, type.
--
-- @
-- ala :: Rewrapping s t => (Unwrapped s -> s) -> ((Unwrapped t -> t) -> e -> s) -> e -> Unwrapped s
-- @

ala :: (Functor f, Rewrapping s t) => (Unwrapped s -> s) -> ((Unwrapped t -> t) -> f s) -> f (Unwrapped s)
ala f = xplat $ _Unwrapping f
{-# INLINE ala #-}

-- | This combinator is based on @ala'@ from Conor McBride's work on Epigram.
--
-- As with '_Wrapping', the user supplied function for the newtype is /ignored/.
--
-- @
-- alaf :: Rewrapping s t => (Unwrapped s -> s) -> ((r -> t) -> e -> s) -> (r -> Unwrapped t) -> e -> Unwrapped s
-- @
--
-- >>> alaf Sum foldMap Prelude.length ["hello","world"]
-- 10
alaf :: (Functor f, Functor g, Rewrapping s t) => (Unwrapped s -> s) -> (f t -> g s) -> f (Unwrapped t) -> g (Unwrapped s)
alaf f = xplatf $ _Unwrapping f
{-# INLINE alaf #-}
