{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Wrapped
-- Copyright   :  (C) 2012 Edward Kmett, Michael Sloan
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2
--
-- The 'Wrapped' class provides similar functionality as \"Control.Newtype\",
-- but in a form more convenient and efficient for lens.
--
----------------------------------------------------------------------------
module Control.Lens.Wrapped
  ( Wrapped(..)
  , wrapping, wrappings
  ) where

import Control.Applicative
import Control.Arrow
import Control.Lens.Iso
import Data.Monoid

-- | 'Wrapped' provides isomorphisms to wrap and unwrap newtypes.
class Wrapped s t a b | a -> s, b -> t, a t -> s, b s -> t where
  wrapped :: Iso s t a b
  unwrapped :: Iso a b s t

-- | Convenience for 'wrapped' with an argument that's ignored.  This argument
--   is used to specify which newtype the user intends to wrap, by using the
--   constructor for that newtype.
wrapping :: Wrapped s s a a => (s -> a) -> Iso s s a a
wrapping _ = wrapped

-- | Convenience for 'wrapped' with two arguments that are ignored.  These
--   arguments are used to which newtype the user intends to wrap.  Usually
--   these arguments would both be the same constructor.  This redundency is
--   necessary in order to have a fully polymorphic isomorphism.
wrappings :: Wrapped s t a b => (s -> a) -> (t -> b) -> Iso s t a b
wrappings _ _ = wrapped

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

getArrowMonad :: ArrowApply m  => ArrowMonad m a -> m () a
getArrowMonad (ArrowMonad x) = x
