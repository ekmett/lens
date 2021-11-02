{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-warnings-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Zoom
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Zoom
  (
  -- * Zoom
    Focusing(..)
  , FocusingWith(..)
  , FocusingPlus(..)
  , FocusingOn(..)
  , FocusingMay(..), May(..)
  , FocusingErr(..), Err(..)
  , FocusingFree(..), Freed(..)
  -- * Magnify
  , Effect(..)
  , EffectRWS(..)
  ) where

import Prelude ()

import Control.Lens.Internal.Prelude
import Control.Monad
import Control.Monad.Trans.Free
import Data.Functor.Bind

------------------------------------------------------------------------------
-- Focusing
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.State.StateT'.
newtype Focusing m s a = Focusing { unfocusing :: m (s, a) }

instance Monad m => Functor (Focusing m s) where
  fmap f (Focusing m) = Focusing $ do
     (s, a) <- m
     return (s, f a)
  {-# INLINE fmap #-}

instance (Monad m, Semigroup s) => Apply (Focusing m s) where
  Focusing mf <.> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (s <> s', f a)
  {-# INLINE (<.>) #-}

instance (Monad m, Monoid s) => Applicative (Focusing m s) where
  pure a = Focusing (return (mempty, a))
  {-# INLINE pure #-}
  Focusing mf <*> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (mappend s s', f a)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingWith
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.RWS.RWST'.
newtype FocusingWith w m s a = FocusingWith { unfocusingWith :: m (s, a, w) }

instance Monad m => Functor (FocusingWith w m s) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (s, a, w) <- m
     return (s, f a, w)
  {-# INLINE fmap #-}

instance (Monad m, Semigroup s, Semigroup w) => Apply (FocusingWith w m s) where
  FocusingWith mf <.> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (s <> s', f a, w <> w')
  {-# INLINE (<.>) #-}

instance (Monad m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure a = FocusingWith (return (mempty, a, mempty))
  {-# INLINE pure #-}
  FocusingWith mf <*> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (mappend s s', f a, mappend w w')
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingPlus
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)
  {-# INLINE fmap #-}

instance Apply (k (s, w)) => Apply (FocusingPlus w k s) where
  FocusingPlus kf <.> FocusingPlus ka = FocusingPlus (kf <.> ka)
  {-# INLINE (<.>) #-}

instance Applicative (k (s, w)) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  {-# INLINE pure #-}
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingOn
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'.
newtype FocusingOn f k s a = FocusingOn { unfocusingOn :: k (f s) a }

instance Functor (k (f s)) => Functor (FocusingOn f k s) where
  fmap f (FocusingOn as) = FocusingOn (fmap f as)
  {-# INLINE fmap #-}

instance Apply (k (f s)) => Apply (FocusingOn f k s) where
  FocusingOn kf <.> FocusingOn ka = FocusingOn (kf <.> ka)
  {-# INLINE (<.>) #-}

instance Applicative (k (f s)) => Applicative (FocusingOn f k s) where
  pure = FocusingOn . pure
  {-# INLINE pure #-}
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- May
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'Maybe' for error handling.
newtype May a = May { getMay :: Maybe a }

instance Semigroup a => Semigroup (May a) where
  May Nothing <> _ = May Nothing
  _ <> May Nothing = May Nothing
  May (Just a) <> May (Just b) = May (Just (a <> b))
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (May a) where
  mempty = May (Just mempty)
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  May Nothing `mappend` _ = May Nothing
  _ `mappend` May Nothing = May Nothing
  May (Just a) `mappend` May (Just b) = May (Just (mappend a b))
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- FocusingMay
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Error.ErrorT'.
newtype FocusingMay k s a = FocusingMay { unfocusingMay :: k (May s) a }

instance Functor (k (May s)) => Functor (FocusingMay k s) where
  fmap f (FocusingMay as) = FocusingMay (fmap f as)
  {-# INLINE fmap #-}

instance Apply (k (May s)) => Apply (FocusingMay k s) where
  FocusingMay kf <.> FocusingMay ka = FocusingMay (kf <.> ka)
  {-# INLINE (<.>) #-}

instance Applicative (k (May s)) => Applicative (FocusingMay k s) where
  pure = FocusingMay . pure
  {-# INLINE pure #-}
  FocusingMay kf <*> FocusingMay ka = FocusingMay (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- Err
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'Either' for error handling.
newtype Err e a = Err { getErr :: Either e a }

instance Semigroup a => Semigroup (Err e a) where
  Err (Left e) <> _ = Err (Left e)
  _ <> Err (Left e) = Err (Left e)
  Err (Right a) <> Err (Right b) = Err (Right (a <> b))
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Err e a) where
  mempty = Err (Right mempty)
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- FocusingErr
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Error.ErrorT'.
newtype FocusingErr e k s a = FocusingErr { unfocusingErr :: k (Err e s) a }

instance Functor (k (Err e s)) => Functor (FocusingErr e k s) where
  fmap f (FocusingErr as) = FocusingErr (fmap f as)
  {-# INLINE fmap #-}

instance Apply (k (Err e s)) => Apply (FocusingErr e k s) where
  FocusingErr kf <.> FocusingErr ka = FocusingErr (kf <.> ka)
  {-# INLINE (<.>) #-}

instance Applicative (k (Err e s)) => Applicative (FocusingErr e k s) where
  pure = FocusingErr . pure
  {-# INLINE pure #-}
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- Freed
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'FreeF' for result collection.

newtype Freed f m a = Freed { getFreed :: FreeF f a (FreeT f m a) }

instance (Applicative f, Semigroup a, Monad m) => Semigroup (Freed f m a) where
  Freed (Pure a) <> Freed (Pure b) = Freed $ Pure $ a <> b
  Freed (Pure a) <> Freed (Free g) = Freed $ Free $ liftA2 (liftM2 (<>)) (pure $ return a) g
  Freed (Free f) <> Freed (Pure b) = Freed $ Free $ liftA2 (liftM2 (<>)) f (pure $ return b)
  Freed (Free f) <> Freed (Free g) = Freed $ Free $ liftA2 (liftM2 (<>)) f g

instance (Applicative f, Monoid a, Monad m) => Monoid (Freed f m a) where
  mempty = Freed $ Pure mempty

#if !(MIN_VERSION_base(4,11,0))
  Freed (Pure a) `mappend` Freed (Pure b) = Freed $ Pure $ a `mappend` b
  Freed (Pure a) `mappend` Freed (Free g) = Freed $ Free $ liftA2 (liftM2 mappend) (pure $ return a) g
  Freed (Free f) `mappend` Freed (Pure b) = Freed $ Free $ liftA2 (liftM2 mappend) f (pure $ return b)
  Freed (Free f) `mappend` Freed (Free g) = Freed $ Free $ liftA2 (liftM2 mappend) f g
#endif

------------------------------------------------------------------------------
-- FocusingFree
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into
-- 'Control.Monad.Trans.FreeT'
newtype FocusingFree f m k s a = FocusingFree { unfocusingFree :: k (Freed f m s) a }

instance Functor (k (Freed f m s)) => Functor (FocusingFree f m k s) where
  fmap f (FocusingFree as) = FocusingFree (fmap f as)
  {-# INLINE fmap #-}

instance Apply (k (Freed f m s)) => Apply (FocusingFree f m k s) where
  FocusingFree kf <.> FocusingFree ka = FocusingFree (kf <.> ka)
  {-# INLINE (<.>) #-}

instance Applicative (k (Freed f m s)) => Applicative (FocusingFree f m k s) where
  pure = FocusingFree . pure
  {-# INLINE pure #-}
  FocusingFree kf <*> FocusingFree ka = FocusingFree (kf <*> ka)
  {-# INLINE (<*>) #-}

-----------------------------------------------------------------------------
--- Effect
-------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }
-- type role Effect representational nominal phantom

instance Functor (Effect m r) where
  fmap _ (Effect m) = Effect m
  {-# INLINE fmap #-}

instance Contravariant (Effect m r) where
  contramap _ (Effect m) = Effect m
  {-# INLINE contramap #-}

instance (Monad m, Semigroup r) => Semigroup (Effect m r a) where
  Effect ma <> Effect mb = Effect (liftM2 (<>) ma mb)
  {-# INLINE (<>) #-}

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)
  {-# INLINE mappend #-}
#endif

instance (Apply m, Semigroup r) => Apply (Effect m r) where
  Effect ma <.> Effect mb = Effect (liftF2 (<>) ma mb)
  {-# INLINE (<.>) #-}

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  {-# INLINE pure #-}
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- EffectRWS
------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument. Used when magnifying 'Control.Monad.RWS.RWST'.
newtype EffectRWS w st m s a = EffectRWS { getEffectRWS :: st -> m (s,st,w) }

instance Functor (EffectRWS w st m s) where
  fmap _ (EffectRWS m) = EffectRWS m
  {-# INLINE fmap #-}

instance (Semigroup s, Semigroup w, Bind m) => Apply (EffectRWS w st m s) where
  EffectRWS m <.> EffectRWS n = EffectRWS $ \st -> m st >>- \ (s,t,w) -> fmap (\(s',u,w') -> (s <> s', u, w <> w')) (n t)
  {-# INLINE (<.>) #-}

instance (Monoid s, Monoid w, Monad m) => Applicative (EffectRWS w st m s) where
  pure _ = EffectRWS $ \st -> return (mempty, st, mempty)
  {-# INLINE pure #-}
  EffectRWS m <*> EffectRWS n = EffectRWS $ \st -> m st >>= \ (s,t,w) -> n t >>= \ (s',u,w') -> return (mappend s s', u, mappend w w')
  {-# INLINE (<*>) #-}

instance Contravariant (EffectRWS w st m s) where
  contramap _ (EffectRWS m) = EffectRWS m
  {-# INLINE contramap #-}
