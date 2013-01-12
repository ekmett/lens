{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Context
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Context
  ( IndexedFunctor(..)
  , IndexedComonad(..)
  , IndexedComonadStore(..)
  , Sellable(..)
  , Context(..), Context'
  , Pretext(..), Pretext'
  , PretextT(..), PretextT'
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- IndexedFunctor
------------------------------------------------------------------------------

class IndexedFunctor w where
  ifmap :: (s -> t) -> w a b s -> w a b t

------------------------------------------------------------------------------
-- IndexedComonad
------------------------------------------------------------------------------

class IndexedFunctor w => IndexedComonad w where
  iextract :: w a a t -> t

  iduplicate :: w a c t -> w a b (w b c t)
  iduplicate = iextend id
  {-# INLINE iduplicate #-}

  iextend :: (w b c t -> r) -> w a c t -> w a b r
  iextend f = ifmap f . iduplicate
  {-# INLINE iextend #-}

------------------------------------------------------------------------------
-- IndexedComonadStore
------------------------------------------------------------------------------

class IndexedComonad w => IndexedComonadStore w where
  ipos :: w a c t -> a

  ipeek :: c  -> w a c t -> t
  ipeek c = iextract . iseek c
  {-# INLINE ipeek #-}

  ipeeks :: (a -> c) -> w a c t -> t
  ipeeks f = iextract . iseeks f
  {-# INLINE ipeeks #-}

  iseek :: b  -> w a c t -> w b c t
  iseeks :: (a -> b) -> w a c t -> w b c t

  iexperiment :: Functor f => (b -> f c) -> w b c t -> f t
  iexperiment bfc wbct = (`ipeek` wbct) <$> bfc (ipos wbct)
  {-# INLINE iexperiment #-}

  context :: w a b t -> Context a b t
  context wabt = Context (`ipeek` wabt) (ipos wabt)
  {-# INLINE context #-}

------------------------------------------------------------------------------
-- Sellable
------------------------------------------------------------------------------

class Corepresentable p => Sellable p w where
  sell :: p a (w a b b)

------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------

-- | The indexed store can be used to characterize a 'Control.Lens.Lens.Lens'
-- and is used by 'Control.Lens.Lens.clone'.
--
-- @'Context' a b t@ is isomorphic to
-- @newtype 'Context' a b t = 'Context' { runContext :: forall f. 'Functor' f => (a -> f b) -> f t }@,
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

instance (p ~ (->)) => Sellable p Context where
  sell = Context id
  {-# INLINE sell #-}

-- | @type 'Context'' a s = 'Context' a a s@
type Context' a = Context a a

------------------------------------------------------------------------------
-- Pretext
------------------------------------------------------------------------------

newtype Pretext p a b t = Pretext { runPretext :: forall f. Functor f => p a (f b) -> f t }

-- | @type 'Pretext'' p a s = 'Pretext' p a a s@
type Pretext' p a = Pretext p a a

instance IndexedFunctor (Pretext p) where
  ifmap f (Pretext k) = Pretext (fmap f . k)
  {-# INLINE ifmap #-}

instance Functor (Pretext p a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance Conjoined p => IndexedComonad (Pretext p) where
  iextract (Pretext m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (Pretext m) = getCompose $ m (Compose #. distrib sell . sell)
  {-# INLINE iduplicate #-}

instance (a ~ b, Conjoined p) => Comonad (Pretext p a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance Conjoined p => IndexedComonadStore (Pretext p) where
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

instance (a ~ b, Conjoined p) => ComonadStore a (Pretext p a b) where
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

instance (Corepresentable p, p ~ p') => Sellable p' (Pretext p) where
  sell = cotabulate $ \ w -> Pretext (`corep` w)
  {-# INLINE sell #-}

------------------------------------------------------------------------------
-- PretextT
------------------------------------------------------------------------------

newtype PretextT p (g :: * -> *) a b t = PretextT { runPretextT :: forall f. Functor f => p a (f b) -> f t }

-- | @type 'PretextT'' p g a s = 'PretextT' p g a a s@
type PretextT' p g a = PretextT p g a a

instance IndexedFunctor (PretextT p g) where
  ifmap f (PretextT k) = PretextT (fmap f . k)
  {-# INLINE ifmap #-}

instance Functor (PretextT p g a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance Conjoined p => IndexedComonad (PretextT p g) where
  iextract (PretextT m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (PretextT m) = getCompose $ m (Compose #. distrib sell . sell)
  {-# INLINE iduplicate #-}

instance (a ~ b, Conjoined p) => Comonad (PretextT p g a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance Conjoined p => IndexedComonadStore (PretextT p g) where
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

instance (a ~ b, Conjoined p) => ComonadStore a (PretextT p g a b) where
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

instance (Corepresentable p, p ~ p') => Sellable p' (PretextT p g) where
  sell = cotabulate $ \ w -> PretextT (`corep` w)
  {-# INLINE sell #-}

instance (Profunctor p, Gettable g) => Gettable (PretextT p g a b) where
  coerce = (<$) (error "coerced PretextT")
  {-# INLINE coerce #-}

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

coarr :: (Representable q, Comonad (Rep q)) => q a b -> a -> b
coarr qab = extract . rep qab
{-# INLINE coarr #-}
