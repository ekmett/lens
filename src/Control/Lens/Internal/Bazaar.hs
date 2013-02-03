{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Bazaar
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Bazaar
  ( Bizarre(..)
  , Bazaar(..), Bazaar'
  , BazaarT(..), BazaarT'
  ) where

import Control.Applicative
import Control.Arrow as Arrow
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Data.Functor.Apply
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Bizarre
------------------------------------------------------------------------------

-- | This class is used to run the various 'Bazaar' variants used in this
-- library.
class Profunctor p => Bizarre p w | w -> p where
  bazaar :: Applicative f => p a (f b) -> w a b t -> f t

------------------------------------------------------------------------------
-- Bazaar
------------------------------------------------------------------------------

-- | This is used to characterize a 'Control.Lens.Traversal.Traversal'.
--
-- a.k.a. indexed Cartesian store comonad, indexed Kleene store comonad, or an indexed 'FunList'.
--
-- <http://twanvl.nl/blog/haskell/non-regular1>
--
-- A 'Bazaar' is like a 'Control.Lens.Traversal.Traversal' that has already been applied to some structure.
--
-- Where a @'Context' a b t@ holds an @a@ and a function from @b@ to
-- @t@, a @'Bazaar' a b t@ holds @N@ @a@s and a function from @N@
-- @b@s to @t@, (where @N@ might be infinite).
--
-- Mnemonically, a 'Bazaar' holds many stores and you can easily add more.
--
-- This is a final encoding of 'Bazaar'.
newtype Bazaar p a b t = Bazaar { runBazaar :: forall f. Applicative f => p a (f b) -> f t }

-- | This alias is helpful when it comes to reducing repetition in type signatures.
--
-- @
-- type 'Bazaar'' p a t = 'Bazaar' p a a t
-- @
type Bazaar' p a = Bazaar p a a

instance IndexedFunctor (Bazaar p) where
  ifmap f (Bazaar k) = Bazaar (fmap f . k)
  {-# INLINE ifmap #-}

instance Conjoined p => IndexedComonad (Bazaar p) where
  iextract (Bazaar m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (Bazaar m) = getCompose $ m (Compose #. distrib sell . sell)
  {-# INLINE iduplicate #-}

instance Corepresentable p => Sellable p (Bazaar p) where
  sell = cotabulate $ \ w -> Bazaar $ tabulate $ \k -> pure (corep k w)
  {-# INLINE sell #-}

instance Profunctor p => Bizarre p (Bazaar p) where
  bazaar g (Bazaar f) = f g
  {-# INLINE bazaar #-}

instance Functor (Bazaar p a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance Apply (Bazaar p a b) where
  Bazaar mf <.> Bazaar ma = Bazaar $ \ pafb -> mf pafb <*> ma pafb
  {-# INLINE (<.>) #-}

instance Applicative (Bazaar p a b) where
  pure a = Bazaar $ \_ -> pure a
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar $ \ pafb -> mf pafb <*> ma pafb
  {-# INLINE (<*>) #-}

instance (a ~ b, Conjoined p) => Comonad (Bazaar p a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, Conjoined p) => ComonadApply (Bazaar p a b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

------------------------------------------------------------------------------
-- BazaarT
------------------------------------------------------------------------------

-- | 'BazaarT' is like 'Bazaar', except that it provides a questionable 'Contravariant' instance
-- To protect this instance it relies on the soundness of another 'Contravariant' type, and usage conventions.
--
-- For example. This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there
-- must be a better way!
newtype BazaarT p (g :: * -> *) a b t = BazaarT { runBazaarT :: forall f. Applicative f => p a (f b) -> f t }

-- | This alias is helpful when it comes to reducing repetition in type signatures.
--
-- @
-- type 'BazaarT'' p g a t = 'BazaarT' p g a a t
-- @
type BazaarT' p g a = BazaarT p g a a

instance IndexedFunctor (BazaarT p g) where
  ifmap f (BazaarT k) = BazaarT (fmap f . k)
  {-# INLINE ifmap #-}

instance Conjoined p => IndexedComonad (BazaarT p g) where
  iextract (BazaarT m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (BazaarT m) = getCompose $ m (Compose #. distrib sell . sell)
  {-# INLINE iduplicate #-}

instance Corepresentable p => Sellable p (BazaarT p g) where
  sell = cotabulate $ \ w -> BazaarT (`corep` w)
  {-# INLINE sell #-}

instance Profunctor p => Bizarre p (BazaarT p g) where
  bazaar g (BazaarT f) = f g
  {-# INLINE bazaar #-}

instance Functor (BazaarT p g a b) where
  fmap = ifmap
  {-# INLINE fmap #-}

instance Apply (BazaarT p g a b) where
  BazaarT mf <.> BazaarT ma = BazaarT $ \ pafb -> mf pafb <*> ma pafb
  {-# INLINE (<.>) #-}

instance Applicative (BazaarT p g a b) where
  pure a = BazaarT $ tabulate $ \_ -> pure (pure a)
  {-# INLINE pure #-}
  BazaarT mf <*> BazaarT ma = BazaarT $ \ pafb -> mf pafb <*> ma pafb
  {-# INLINE (<*>) #-}

instance (a ~ b, Conjoined p) => Comonad (BazaarT p g a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, Conjoined p) => ComonadApply (BazaarT p g a b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

instance (Profunctor p, Contravariant g) => Contravariant (BazaarT p g a b) where
  contramap _ = (<$) (error "contramap: BazaarT")
  {-# INLINE contramap #-}
