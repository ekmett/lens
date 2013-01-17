{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Error.Lens
-- Copyright   :  (C) 2013 Edward Kmett
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
  -- * Throwing
  , throwing
  ) where

import Control.Lens
import Control.Monad.Error

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
catching :: MonadError e m => Getting (Leftmost a) e t a b -> m r -> (a -> m r) -> m r
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
catching_ :: MonadError e m => Getting (Leftmost a) e t a b -> m r -> m r -> m r
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
handling :: MonadError e m => Getting (Leftmost a) e t a b -> (a -> m r) -> m r -> m r
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
handling_ :: MonadError e m => Getting (Leftmost a) e t a b -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

------------------------------------------------------------------------------
-- Trying
------------------------------------------------------------------------------

-- 'trying' takes a 'Prism' (or any 'Getter') to select which exceptions are caught 
-- If the 'Exception' does not match the predicate, it is re-thrown.
--
-- @
-- 'trying' :: 'MonadError' e m => 'Prism'' e a     -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Lens'' e a      -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Traversal'' e a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Iso'' e a       -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Getter'' e a    -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadError' e m => 'Fold'' e a      -> m r -> m ('Either' a r)
-- @
trying :: MonadError e m => Getting (Leftmost a) e t a b -> m r -> m (Either a r)
trying l m = catching l (liftM Right m) (return . Left)

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
throwing :: MonadError e m => AReview e e t t -> t -> m x
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

