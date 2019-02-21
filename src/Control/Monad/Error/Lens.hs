{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Error.Lens
-- Copyright   :  (C) 2014-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Control.Monad.Error
--
----------------------------------------------------------------------------
module Control.Monad.Error.Lens
  (
  -- * Catching
    catching, catching_
  -- * Handling
  , handling, handling_
  -- * Trying
  , trying
  -- * Handlers
  , catches
  , Handler(..)
  , Handleable(..)
  -- * Throwing
  , throwing, throwing_
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Exception
import Control.Monad
import Control.Monad.Error.Class
import Data.Functor.Plus
import qualified Data.Monoid as M

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

#ifdef HLINT
{-# ANN module "HLint: ignore Use fmap" #-}
#endif

------------------------------------------------------------------------------
-- Catching
------------------------------------------------------------------------------

-- | Catch exceptions that match a given 'Prism' (or any 'Getter', really).
--
-- @
-- 'catching' :: 'MonadError' e m => 'Prism'' e a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadError' e m => 'Lens'' e a      -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadError' e m => 'Traversal'' e a -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadError' e m => 'Iso'' e a       -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadError' e m => 'Getter' e a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadError' e m => 'Fold' e a       -> m r -> (a -> m r) -> m r
-- @
catching :: MonadError e m => Getting (M.First a) e a -> m r -> (a -> m r) -> m r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- | Catch exceptions that match a given 'Prism' (or any 'Getter'), discarding
-- the information about the match. This is particuarly useful when you have
-- a @'Prism'' e ()@ where the result of the 'Prism' or 'Fold' isn't
-- particularly valuable, just the fact that it matches.
--
-- @
-- 'catching_' :: 'MonadError' e m => 'Prism'' e a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadError' e m => 'Lens'' e a      -> m r -> m r -> m r
-- 'catching_' :: 'MonadError' e m => 'Traversal'' e a -> m r -> m r -> m r
-- 'catching_' :: 'MonadError' e m => 'Iso'' e a       -> m r -> m r -> m r
-- 'catching_' :: 'MonadError' e m => 'Getter' e a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadError' e m => 'Fold' e a       -> m r -> m r -> m r
-- @
catching_ :: MonadError e m => Getting (M.First a) e a -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

------------------------------------------------------------------------------
-- Handling
------------------------------------------------------------------------------

-- | A version of 'catching' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- @
-- 'handling' :: 'MonadError' e m => 'Prism'' e a     -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadError' e m => 'Lens'' e a      -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadError' e m => 'Traversal'' e a -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadError' e m => 'Iso'' e a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadError' e m => 'Fold' e a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadError' e m => 'Getter' e a     -> (a -> m r) -> m r -> m r
-- @
handling :: MonadError e m => Getting (M.First a) e a -> (a -> m r) -> m r -> m r
handling l = flip (catching l)
{-# INLINE handling #-}

-- | A version of 'catching_' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- @
-- 'handling_' :: 'MonadError' e m => 'Prism'' e a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadError' e m => 'Lens'' e a      -> m r -> m r -> m r
-- 'handling_' :: 'MonadError' e m => 'Traversal'' e a -> m r -> m r -> m r
-- 'handling_' :: 'MonadError' e m => 'Iso'' e a       -> m r -> m r -> m r
-- 'handling_' :: 'MonadError' e m => 'Getter' e a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadError' e m => 'Fold' e a       -> m r -> m r -> m r
-- @
handling_ :: MonadError e m => Getting (M.First a) e a -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

------------------------------------------------------------------------------
-- Trying
------------------------------------------------------------------------------

-- | 'trying' takes a 'Prism' (or any 'Getter') to select which exceptions are caught
-- If the 'Exception' does not match the predicate, it is re-thrown.
--
-- @
-- 'trying' :: 'MonadError' e m => 'Prism'' e a     -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Lens'' e a      -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Traversal'' e a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Iso'' e a       -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Getter' e a     -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Fold' e a       -> m r -> m ('Either' a r)
-- @
trying :: MonadError e m => Getting (M.First a) e a -> m r -> m (Either a r)
trying l m = catching l (liftM Right m) (return . Left)

------------------------------------------------------------------------------
-- Catches
------------------------------------------------------------------------------

-- |
-- This function exists to remedy a gap between the functionality of @Control.Exception@
-- and @Control.Monad.Error@. @Control.Exception@ supplies 'Control.Exception.catches' and
-- a notion of 'Control.Exception.Handler', which we duplicate here in a form suitable for
-- working with any 'MonadError' instance.
--
-- Sometimes you want to catch two different sorts of error. You could
-- do something like
--
-- @
-- f = 'handling' _Foo handleFoo ('handling' _Bar handleBar expr)
-- @
--
--
-- However, there are a couple of problems with this approach. The first is
-- that having two exception handlers is inefficient. However, the more
-- serious issue is that the second exception handler will catch exceptions
-- in the first, e.g. in the example above, if @handleFoo@ uses 'throwError'
-- then the second exception handler will catch it.
--
-- Instead, we provide a function 'catches', which would be used thus:
--
-- @
-- f = 'catches' expr [ 'handler' _Foo handleFoo
--                  , 'handler' _Bar handleBar
--                  ]
-- @
catches :: MonadError e m => m a -> [Handler e m a] -> m a
catches m hs = catchError m go where
  go e = foldr tryHandler (throwError e) hs where
    tryHandler (Handler ema amr) res = maybe res amr (ema e)

------------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------------

-- | You need this when using 'catches'.
data Handler e m r = forall a. Handler (e -> Maybe a) (a -> m r)

instance Monad m => Functor (Handler e m) where
  fmap f (Handler ema amr) = Handler ema $ \a -> do
     r <- amr a
     return (f r)
  {-# INLINE fmap #-}

instance Monad m => Semigroup (Handler e m a) where
  (<>) = M.mappend
  {-# INLINE (<>) #-}

instance Monad m => Alt (Handler e m) where
  Handler ema amr <!> Handler emb bmr = Handler emab abmr where
    emab e = Left <$> ema e <|> Right <$> emb e
    abmr = either amr bmr
  {-# INLINE (<!>) #-}

instance Monad m => Plus (Handler e m) where
  zero = Handler (const Nothing) undefined
  {-# INLINE zero #-}

instance Monad m => M.Monoid (Handler e m a) where
  mempty = zero
  {-# INLINE mempty #-}
  mappend = (<!>)
  {-# INLINE mappend #-}

instance Handleable e m (Handler e m) where
  handler = Handler . preview
  {-# INLINE handler #-}

------------------------------------------------------------------------------
-- Throwing
------------------------------------------------------------------------------

-- | Throw an 'Exception' described by a 'Prism'.
--
-- @'throwing' l ≡ 'reviews' l 'throwError'@
--
-- @
-- 'throwing' :: 'MonadError' e m => 'Prism'' e t -> t -> a
-- 'throwing' :: 'MonadError' e m => 'Iso'' e t   -> t -> a
-- @
throwing :: MonadError e m => AReview e t -> t -> m x
throwing l = reviews l throwError
{-# INLINE throwing #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

-- | Helper function to provide conditional catch behavior.
catchJust :: MonadError e m => (e -> Maybe t) -> m a -> (t -> m a) -> m a
catchJust f m k = catchError m $ \ e -> case f e of
  Nothing -> throwError e
  Just x  -> k x
{-# INLINE catchJust #-}

-- | Similar to 'throwing' but specialised for the common case of
--   error constructors with no arguments.
--
-- @
-- data MyError = Foo | Bar
-- makePrisms ''MyError
-- 'throwing_' _Foo :: 'MonadError' MyError m => m a
-- @
throwing_ :: MonadError e m => AReview e () -> m x
throwing_ l = throwing l ()
{-# INLINE throwing_ #-}
