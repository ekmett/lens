{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Action
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, FDs, Rank2
--
----------------------------------------------------------------------------
module Control.Lens.Action
  (
  -- * Composable Actions
    Action
  , act
  , acts
  , perform
  , liftAct
  , (^!)

  -- * Folds with Effecs
  , MonadicFold

  -- * Implementation Details
  , Acting
  , Effective(..)
  , ineffective
  , Effect(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Getter
import Control.Lens.Iso
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Monoid

infixr 8 ^!

-- | An 'Action' is a 'Getter' enriched with access to a 'Monad' for side-effects.
--
-- Every 'Getter' can be used as an 'Action'
--
-- You can compose an 'Action' with another 'Action' using ('Prelude..') from the @Prelude@.
type Action m a c = forall f b r d. Effective m r f => (c -> f d) -> a -> f b

-- | A 'MonadicFold' is a 'Fold' enriched with access to a 'Monad' for side-effects.
--
-- Every 'Fold' can be used as a 'MonadicFold', that simply ignores the access to the 'Monad'.
--
-- You can compose a 'MonadicFold' with another 'MonadicFold' using ('Prelude..') from the @Prelude@.
type MonadicFold m a c = forall f b r d. (Effective m r f, Applicative f) => (c -> f d) -> a -> f b

-- | An 'Effective' 'Functor' ignores its argument and is isomorphic to a monad wrapped around a value.
--
-- That said, the monad is possibly rather unrelated to any 'Applicative' structure.
class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: Isomorphic k => k (m r) (f a)

-- | A convenient antonym that is used internally.
ineffective :: Effective m r f => Isomorphic k => k (f a) (m r)
ineffective = from effective
{-# INLINE ineffective #-}

instance Effective Identity r (Accessor r) where
  effective = isomorphic (Accessor . runIdentity) (Identity . runAccessor)
  {-# INLINE effective #-}
  {-# SPECIALIZE effective :: Identity r -> Accessor r a #-}
  {-# SPECIALIZE effective :: Isomorphism (Identity r) (Accessor r a) #-}

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = isomorphic (Backwards . effective . liftM getDual) (liftM Dual . ineffective . forwards)

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }

instance Monad m => Functor (Effect m r) where
  fmap _ (Effect m) = Effect m

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)

instance Monad m => Gettable (Effect m r) where
  coerce (Effect m) = Effect m

instance Monad m => Effective m r (Effect m r) where
  effective = isomorphic Effect getEffect
  {-# SPECIALIZE effective :: Monad m => m r -> Effect m r a #-}
  {-# SPECIALIZE effective :: Monad m => Isomorphism (m r) (Effect m r a) #-}

-- | Used to evaluate an 'Action'.
type Acting m r a b c d = (c -> Effect m r d) -> a -> Effect m r b

-- | Perform an 'Action'.
--
-- > perform = flip (^!)
--
perform :: Monad m => Acting m c a b c d -> a -> m c
perform l = getEffect . l (Effect . return)
{-# INLINE perform #-}

-- | Perform an 'Action'
--
-- >>> import Control.Lens
--
-- >>> ["hello","world"]^!folded.act putStrLn
-- hello
-- world
--
(^!) :: Monad m => a -> Acting m c a b c d -> m c
a ^! l = getEffect (l (Effect . return) a)
{-# INLINE (^!) #-}

-- | Construct an 'Action' from a monadic side-effect
act :: Monad m => (a -> m c) -> Action m a c
act amc cfd a = effective (amc a >>= from effective . cfd)
{-# INLINE act #-}

-- | A self-running 'Action', analogous to 'Control.Monad.join'.
--
-- @'acts' = 'act' 'id'@
--
-- >>> import Control.Lens
--
-- >>> (1,"hello")^!_2.acts.to succ
-- "ifmmp"
acts :: Action m (m a) a
acts = act id
{-# INLINE acts #-}

-- | Apply a 'Monad' transformer to an 'Action'.
liftAct :: (MonadTrans t, Monad m) => Acting m c a b c d -> Action (t m) a c
liftAct l = act (lift . perform l)
{-# INLINE liftAct #-}
