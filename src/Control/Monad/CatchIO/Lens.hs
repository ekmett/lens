{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.CatchIO.Lens
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Monad.CatchIO.Lens
  (
  -- * Catching
    catching, catching_
  -- * Handling
  , handling, handling_
  -- * Trying
  , trying
  -- * Throwing
  , throwing
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.CatchIO as CatchIO hiding (try, tryJust)
import Control.Exception (SomeException)
import Prelude (asTypeOf, const, flip, undefined, ($), (.),  Maybe(..), Either(..), Functor(..))

------------------------------------------------------------------------------
-- Catching
------------------------------------------------------------------------------

-- | Catch exceptions that match a given 'Prism' (or any 'Getter', really).
--
-- @
-- 'catching' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> m r -> (a -> m r) -> m r
-- @
catching :: MonadCatchIO m => Getting (Leftmost a) SomeException t a b -> m r -> (a -> m r) -> m r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- | Catch exceptions that match a given 'Prism' (or any 'Getter'), discarding
-- the information about the match. This is particuarly useful when you have
-- a @'Prism'' e ()@ where the result of the 'Prism' or 'Fold' isn't
-- particularly valuable, just the fact that it matches.
--
-- @
-- 'catching_' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> m r -> m r -> m r
-- @
catching_ :: MonadCatchIO m => Getting (Leftmost a) SomeException t a b -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

------------------------------------------------------------------------------
-- Handling
------------------------------------------------------------------------------

-- | A version of 'catching' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- @
-- 'handling' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> (a -> m r) -> m r -> m r
-- @
handling :: MonadCatchIO m => Getting (Leftmost a) SomeException t a b -> (a -> m r) -> m r -> m r
handling l = flip (catching l)
{-# INLINE handling #-}

-- | A version of 'catching_' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- @
-- 'handling_' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> m r -> m r -> m r
-- @
handling_ :: MonadCatchIO m => Getting (Leftmost a) SomeException t a b -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

------------------------------------------------------------------------------
-- Trying
------------------------------------------------------------------------------

-- A variant of 'try' that takes an 'Prism' (or any 'Getter') to select which
-- exceptions are caught (c.f. 'tryJust', 'catchJust'). If the 'Exception' does
-- not match the predicate, it is re-thrown.
--
-- @
-- 'trying' :: 'MonadCatchIO' m => 'Prism''     'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Lens''      'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Iso''       'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Getter''    'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Fold''      'SomeException' a -> m r -> m ('Either' a r)
-- @
trying :: MonadCatchIO m => Getting (Leftmost a) SomeException t a b -> m r -> m (Either a r)
trying l = tryJust (preview l)


------------------------------------------------------------------------------
-- Throwing
------------------------------------------------------------------------------

-- | Throw an 'Exception' described by a 'Prism'.
--
-- @'throwing' l ≡ 'reviews' l 'throw'@
--
-- @
-- 'throwing' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' t -> t -> a
-- 'throwing' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' t   -> t -> a
-- @
throwing :: MonadCatchIO m => AReview' SomeException t -> t -> m x
throwing l = reviews l throw
{-# INLINE throwing #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

-- | Helper function to provide conditional catch behavior.
catchJust :: (MonadCatchIO m, Exception e) => (e -> Maybe t) -> m a -> (t -> m a) -> m a
catchJust f m k = catch m $ \ e -> case f e of
  Nothing -> throw e
  Just x  -> k x
{-# INLINE catchJust #-}

-- | A version of 'CatchIO.try' that doesn't needlessly require 'Functor'
try :: (MonadCatchIO m, Exception e) => m a -> m (Either e a)
try a = catch (liftM Right a) (return . Left)

-- | A version of 'CatchIO.tryJust' that doesn't needlessly require 'Functor'
tryJust :: (MonadCatchIO m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p a = do
  r <- try a
  case r of
    Right v -> return (Right v)
    Left  e -> case p e of
      Nothing -> throw e `asTypeOf` return (Left undefined)
      Just b  -> return (Left b)
