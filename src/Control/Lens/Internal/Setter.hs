{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Setter
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Setter
  (
  -- ** Setters
    Settable(..)
  , Mutator(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Comonad
import Data.Distributive
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Extend
import Data.Functor.Identity
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Traversable

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class (Applicative f, Distributive f, Traversable f) => Settable f where
  untainted :: f a -> a

  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot g = g `seq` rmap untainted g
  {-# INLINE untaintedDot #-}

  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot g = g `seq` rmap pure g
  {-# INLINE taintedDot #-}

-- | So you can pass our 'Control.Lens.Setter.Setter' into combinators from other lens libraries.
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = (runIdentity #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Identity #.)
  {-# INLINE taintedDot #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untaintedDot forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untaintedDot (untaintedDot getCompose)
  {-# INLINE untainted #-}

-----------------------------------------------------------------------------
-- Mutator
-----------------------------------------------------------------------------

-- | 'Mutator' is just a renamed 'Identity' 'Functor' to give better error
-- messages when someone attempts to use a 'Control.Lens.Getter.Getter' as a 'Control.Lens.Setter.Setter'.
--
-- Most user code will never need to see this type.
newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator (f a)
  {-# INLINE fmap #-}

instance Apply Mutator where
  Mutator f <.> Mutator a = Mutator (f a)
  {-# INLINE (<.>) #-}

instance Applicative Mutator where
  pure = Mutator
  {-# INLINE pure #-}
  Mutator f <*> Mutator a = Mutator (f a)
  {-# INLINE (<*>) #-}

instance Bind Mutator where
  Mutator x >>- f = f x
  {-# INLINE (>>-) #-}
  join = runMutator
  {-# INLINE join #-}

instance Monad Mutator where
  return = Mutator
  {-# INLINE return #-}
  Mutator x >>= f = f x
  {-# INLINE (>>=) #-}

instance Extend Mutator where
  extended f w = Mutator (f w)
  {-# INLINE extended #-}
  duplicated = Mutator
  {-# INLINE duplicated #-}

instance Comonad Mutator where
  extract = runMutator
  {-# INLINE extract #-}
  extend f w = Mutator (f w)
  {-# INLINE extend #-}
  duplicate = Mutator
  {-# INLINE duplicate #-}

instance ComonadApply Mutator where
  Mutator f <@> Mutator a = Mutator (f a)
  {-# INLINE (<@>) #-}

instance Distributive Mutator where
  distribute = Mutator . fmap runMutator
  {-# INLINE distribute #-}

instance Foldable Mutator where
  foldMap f (Mutator a) = f a
  {-# INLINE foldMap #-}

instance Traversable Mutator where
  traverse f (Mutator a) = Mutator <$> f a
  {-# INLINE traverse #-}

instance Settable Mutator where
  untainted = runMutator
  {-# INLINE untainted #-}
  untaintedDot = (runMutator #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Mutator #.)
  {-# INLINE taintedDot #-}
