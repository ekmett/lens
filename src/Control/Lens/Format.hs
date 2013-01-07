{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Format
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Format
  (
  -- * Formats
    Format
  , Formatting, Formatting'
  -- * Composable Formats
  , run
  , now
  , later
  -- * Implementation Details
  , Formattable(..)
  , Form(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal
import Data.Distributive
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Profunctor.Rep
import Unsafe.Coerce

------------------------------------------------------------------------------
-- Formattable
------------------------------------------------------------------------------

class Profunctor p => Formattable m p where
  formatted :: p a b -> p a (m -> b)

instance Formattable m (->) where
  formatted ab a _ = ab a
  {-# INLINE formatted #-}

instance (m ~ n, Monoid n) => Formattable n (Indexed m) where
  formatted (Indexed mab) = Indexed (\m a n -> mab (mappend m n) a)
  {-# INLINE formatted #-}

------------------------------------------------------------------------------
-- Reviewable
------------------------------------------------------------------------------

newtype Form m a b = Form { runForm :: m -> b }

instance (m ~ n, Monoid n) => Formattable n (Form m) where
  formatted (Form mb) = Form (\m n -> mb (mappend m n))
  {-# INLINE formatted #-}

instance Reviewable (Form m) where
  retagged (Form m) = Form m

instance Profunctor (Form m) where
  lmap _ (Form mb) = Form mb
  rmap bc (Form mb) = Form (bc . mb)
  Form mb .# _ = Form mb
  ( #. ) _ = unsafeCoerce

instance Corepresentable (Form m) where
  type Corep (Form m) = Const m
  cotabulate f = Form (f .# Const)
  corep (Form k) = k .# getConst

instance Prismatic (Form m) where
  prismatic (Form k) = Form k

------------------------------------------------------------------------------
-- Formats
------------------------------------------------------------------------------

type Format m t b = forall p f. (Reviewable p, Formattable m p, Settable f) => Overloaded' p f t b

type Formatting m n s t a b = Overloading (Form m) (Form n) Mutator s t a b

type Formatting' m n t b = Formatting m n t t b b

------------------------------------------------------------------------------
-- Formattable
------------------------------------------------------------------------------

later :: (Formattable m p, Reviewable p, Distributive f) => (x -> m) -> Overloaded' p f (x -> b) b
later f = unto (. f) . rmap distribute . formatted

run :: Monoid m => Formatting b m s t a b -> t
run l = (runMutator #. runForm (l (Form Mutator))) mempty

now :: Formattable m p => m -> Overloaded p f a b a b
now m = rmap ($ m) . formatted

{-
lighter :: (Formattable m p, Reviewable p, Distributive f) => ((m -> b) -> t) -> Overloaded' p f t b
lighter f = unto f . rmap distribute . formatted

resume :: Formatting b m s t a b -> m -> t
resume l = runMutator #. runForm (l (Form Mutator))

rip :: Monoid m => Overloading (Indexed n) (Indexed m) f s t a b -> (n -> a -> f b) -> s -> f t
rip l f = runIndexed (l (Indexed f)) mempty

rumble :: Monoid i => (Indexed j s (Mutator t) -> Indexed i a (Mutator b)) -> (j -> s -> t) -> a -> b
rumble l f = runMutator #. runIndexed (l (Indexed (\i -> Mutator #. f i))) mempty

litter :: (Formattable m p, Distributive f) => (x -> m) -> Overloaded p f a (x -> y) a y
litter f = rmap (cotraverse (. f)) . formatted

jcompose :: (Indexable i q, Indexable k p) => (i -> j -> k) -> (Indexed j s t -> q a b) -> p s t -> Indexed i a b
jcompose ijk jab2ist kab = Indexed $ \i -> indexed (jab2ist (Indexed $ \j -> indexed kab (ijk i j))) i
-}
