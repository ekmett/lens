{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Classes
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Control.Lens.Classes
  (
  -- * Getters
    Gettable(..)
  , noEffect
  -- * Actions
  , Effective(..)
  -- * Setters
  , Settable(..)
  -- * Isomorphisms
  , Isomorphic(..)
  -- * Projections
  , Projective(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Category
import Control.Monad (liftM)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid
import Unsafe.Coerce
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | Generalizing 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages
--
-- A 'Gettable' 'Functor' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- To ensure this, an instance of 'Gettable' is required to satisfy:
--
-- @'id' = 'fmap' f = 'coerce'@
--
-- Which is equivalent to making a @'Gettable' f@ an \"anyvariant\" functor.
--

class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Applicative f, Gettable f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}

-------------------------------------------------------------------------------
-- Programming with Effects
-------------------------------------------------------------------------------

-- | An 'Effective' 'Functor' ignores its argument and is isomorphic to a monad wrapped around a value.
--
-- That said, the monad is possibly rather unrelated to any 'Applicative' structure.
class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: m r -> f a
  ineffective :: f a -> m r

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = Backwards . effective . liftM getDual
  ineffective = liftM Dual . ineffective . forwards

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class Applicative f => Settable f where
  untainted :: f a -> a

  untainted# :: (a -> f b) -> a -> b
  untainted# f = untainted . f

  tainted# :: (a -> b) -> a -> f b
  tainted# f = pure . f

-- | so you can pass our a 'Control.Lens.Setter.Setter' into combinators from other lens libraries
instance Settable Identity where
  untainted = runIdentity
  untainted# = unsafeCoerce
  {-# INLINE untainted #-}
  tainted# = unsafeCoerce
  {-# INLINE tainted# #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untainted . forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untainted . untainted . getCompose
  {-# INLINE untainted #-}

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | Used to provide overloading of isomorphism application
--
-- An instance of 'Isomorphic' is a 'Category' with a canonical mapping to it from the
-- category of isomorphisms over Haskell types.
class Category k => Isomorphic k where

  -- | Build an isomorphism family from two (related) pairs of inverse functions.
  --
  -- @
  -- 'Control.Lens.Getter.view' ('isos' sa as tb bt) ≡ sa
  -- 'Control.Lens.Getter.view' ('Control.Lens.Iso.from' ('isos' sa as tb bt)) ≡ as
  -- 'Control.Lens.Setter.set' ('isos' sa as tb bt) ab ≡ bt '.' ab '.' sa
  -- 'Control.Lens.Setter.set' ('Control.Lens.Iso.from' ('isos' ac ca bd db)) ab ≡ bd '.' ab '.' ca
  -- @
  --
  -- @isos :: (s -> a) -> (a -> s) -> (t -> b) -> (b -> t) -> 'Iso' s t a b@
  isos :: Functor f => (s -> a) -> (a -> s) -> (t -> b) -> (b -> t) -> k (a -> f b) (s -> f t)

instance Isomorphic (->) where
  isos sa _ _ bt afb s = bt <$> afb (sa s)

-----------------------------------------------------------------------------
-- Projections
-----------------------------------------------------------------------------

-- | Used to provide overloading of projections.
--
-- An instance of 'Projective' is a 'Category' with a canonical mapping to it from the category
-- of embedding-projection pairs over Haskell types.
class Isomorphic k => Projective k where
  -- | Build a 'Control.Lens.Projection.Projection' or 'Control.Lens.Traversal.Traversal'
  projecting :: Applicative f => (b -> t) -> ((a -> f b) -> s -> f t) -> k (a -> f b) (s -> f t)

instance Projective (->) where
  projecting _ k = k
