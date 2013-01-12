{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Review
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Review
  (
  -- ** Reviewing
    Reviewable(..)
  , Reviewed(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Fix
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Distributive
import Data.Foldable
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Tagged
import Data.Traversable
#ifndef SAFE
import Unsafe.Coerce
#endif

-----------------------------------------------------------------------------
-- Reviewable
----------------------------------------------------------------------------

-- | This provides a dual notion to that of 'Gettable'.
class Profunctor p => Reviewable p where
  retagged :: p a b -> p s b
  -- retaggedDot, dotRetagged?

instance Reviewable Tagged where
  retagged = retag
  {-# INLINE retagged #-}

------------------------------------------------------------------------------
-- Review: Reviewed
------------------------------------------------------------------------------

newtype Reviewed a b = Reviewed { runReviewed :: b }

instance Reviewable Reviewed where
  retagged (Reviewed b) = Reviewed b
  {-# INLINE retagged #-}

instance Functor (Reviewed a) where
  fmap bc (Reviewed b) = Reviewed (bc b)
  {-# INLINE fmap #-}

instance Applicative (Reviewed a) where
  pure = Reviewed
  (<*>) a = Reviewed #. runReviewed a .# runReviewed
  {-# INLINE (<*>) #-}
  a <* _ = a
  {-# INLINE (<*) #-}
  _ *> b = b
  {-# INLINE (*>) #-}

instance Comonad (Reviewed a) where
  extract = runReviewed
  {-# INLINE extract #-}
  duplicate = Reviewed
  {-# INLINE duplicate #-}
  extend = (#.) Reviewed
  {-# INLINE extend #-}

instance ComonadApply (Reviewed a) where
  (<@>) a = Reviewed #. runReviewed a .# runReviewed
  {-# INLINE (<@>) #-}
  a <@ _ = a
  {-# INLINE (<@) #-}
  _ @> b = b
  {-# INLINE (@>) #-}

instance Monad (Reviewed a) where
  return = Reviewed
  {-# INLINE return #-}
  Reviewed a >>= f = f a
  {-# INLINE (>>=) #-}
  _ >> a = a
  {-# INLINE (>>) #-}

instance MonadFix (Reviewed a) where
  mfix f = a where a = f (runReviewed a)
  {-# INLINE mfix #-}

instance Foldable (Reviewed a) where
  foldMap f (Reviewed b) = f b
  {-# INLINE foldMap #-}

instance Traversable (Reviewed a) where
  traverse f (Reviewed b) = Reviewed <$> f b
  {-# INLINE traverse #-}

instance Bifunctor Reviewed where
  bimap _ g (Reviewed b) = Reviewed (g b)
  {-# INLINE bimap #-}

instance Bifoldable Reviewed where
  bifoldMap _ g (Reviewed b) = g b
  {-# INLINE bifoldMap #-}

instance Bitraversable Reviewed where
  bitraverse _ g (Reviewed b) = Reviewed <$> g b
  {-# INLINE bitraverse #-}

instance Distributive (Reviewed a) where
  distribute = Reviewed . fmap runReviewed
  {-# INLINE distribute #-}

instance Profunctor Reviewed where
  dimap _ f (Reviewed c) = Reviewed (f c)
  {-# INLINE dimap #-}
  lmap _ (Reviewed c) = Reviewed c
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}
  Reviewed b .# _ = Reviewed b
  {-# INLINE ( .# ) #-}
#ifndef SAFE
  ( #. ) _ = unsafeCoerce
  {-# INLINE ( #. ) #-}
#endif

instance Prismatic Reviewed where
  prismatic (Reviewed b) = Reviewed b
  {-# INLINE prismatic #-}

instance Corepresentable Reviewed where
  type Corep Reviewed = Proxy
  cotabulate f = Reviewed (f Proxy)
  {-# INLINE cotabulate #-}
  corep (Reviewed b) Proxy = b
  {-# INLINE corep #-}
