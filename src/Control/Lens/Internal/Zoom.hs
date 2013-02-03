{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Zoom
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Zoom
  (
  -- * Zoom
    Zoomed
  , Focusing(..)
  , FocusingWith(..)
  , FocusingPlus(..)
  , FocusingOn(..)
  , FocusingMay(..), May(..)
  , FocusingErr(..), Err(..)
  -- * Magnify
  , Magnified
  , EffectRWS(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Action
import Control.Lens.Internal.Getter
import Control.Monad.Reader as Reader
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Functor.Bind
import Data.Functor.Contravariant
import Data.Semigroup
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Zoomed
------------------------------------------------------------------------------

-- | This type family is used by 'Control.Lens.Zoom.Zoom' to describe the common effect type.
type family Zoomed (m :: * -> *) :: * -> * -> *
type instance Zoomed (Strict.StateT s z) = Focusing z
type instance Zoomed (Lazy.StateT s z) = Focusing z
type instance Zoomed (ReaderT e m) = Zoomed m
type instance Zoomed (IdentityT m) = Zoomed m
type instance Zoomed (Strict.RWST r w s z) = FocusingWith w z
type instance Zoomed (Lazy.RWST r w s z) = FocusingWith w z
type instance Zoomed (Strict.WriterT w m) = FocusingPlus w (Zoomed m)
type instance Zoomed (Lazy.WriterT w m) = FocusingPlus w (Zoomed m)
type instance Zoomed (ListT m) = FocusingOn [] (Zoomed m)
type instance Zoomed (MaybeT m) = FocusingMay (Zoomed m)
type instance Zoomed (ErrorT e m) = FocusingErr e (Zoomed m)

------------------------------------------------------------------------------
-- Focusing
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.State.StateT'.
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

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.RWS.RWST'.
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

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Writer.WriterT'.
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

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'.
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
  May Nothing `mappend` _ = May Nothing
  _ `mappend` May Nothing = May Nothing
  May (Just a) `mappend` May (Just b) = May (Just (mappend a b))
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- FocusingMay
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Error.ErrorT'.
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
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- FocusingErr
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Lens.Zoom' to 'Control.Lens.Lens.zoom' into 'Control.Monad.Error.ErrorT'.
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
-- Magnified
------------------------------------------------------------------------------

-- | This type family is used by 'Control.Lens.Zoom.Magnify' to describe the common effect type.
type family Magnified (m :: * -> *) :: * -> * -> *
type instance Magnified (ReaderT b m) = Effect m
type instance Magnified ((->)b) = Accessor
type instance Magnified (Strict.RWST a w s m) = EffectRWS w s m
type instance Magnified (Lazy.RWST a w s m) = EffectRWS w s m
type instance Magnified (IdentityT m) = Magnified m

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
