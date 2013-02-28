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
  -- * Internal Classes
    Reviewable,
  -- * Reviews
    retagged,
    Reviewed(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Fix
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Distributive
import Data.Foldable
import Data.Functor.Bind
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Traversable
import Data.Void
#ifndef SAFE
import Unsafe.Coerce
#endif

-- | This class is provided mostly for backwards compatibility with lens 3.8,
-- but it can also shorten type signatures.
class (Profunctor p, Bifunctor p) => Reviewable p
instance (Profunctor p, Bifunctor p) => Reviewable p

------------------------------------------------------------------------------
-- Review: Reviewed
------------------------------------------------------------------------------

-- | This is a profunctor used internally to implement "Review"
--
-- It plays a role similar to that of 'Control.Lens.Internal.Getter.Accessor'
-- or 'Const' do for "Control.Lens.Getter"
retagged :: (Profunctor p, Bifunctor p) => p a b -> p s b
retagged = first absurd . lmap absurd

newtype Reviewed a b = Reviewed { runReviewed :: b }

instance Functor (Reviewed a) where
  fmap bc (Reviewed b) = Reviewed (bc b)
  {-# INLINE fmap #-}

instance Apply (Reviewed a) where
  (<.>) a = Reviewed #. runReviewed a .# runReviewed
  {-# INLINE (<.>) #-}
  a <. _ = a
  {-# INLINE (<.) #-}
  _ .> b = b
  {-# INLINE (.>) #-}

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
  extend = ( #. ) Reviewed
  {-# INLINE extend #-}

instance ComonadApply (Reviewed a) where
  (<@>) a = Reviewed #. runReviewed a .# runReviewed
  {-# INLINE (<@>) #-}
  a <@ _ = a
  {-# INLINE (<@) #-}
  _ @> b = b
  {-# INLINE (@>) #-}

instance Bind (Reviewed a) where
  Reviewed a >>- f = f a
  {-# INLINE (>>-) #-}

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

instance Choice Reviewed where
  left' (Reviewed b) = Reviewed (Left b)
  {-# INLINE left' #-}
  right' (Reviewed b) = Reviewed (Right b)
  {-# INLINE right' #-}

instance Corepresentable Reviewed where
  type Corep Reviewed = Proxy
  cotabulate f = Reviewed (f Proxy)
  {-# INLINE cotabulate #-}
  corep (Reviewed b) Proxy = b
  {-# INLINE corep #-}
