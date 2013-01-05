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
  -- * Handling
    catching, catching_
  , handling, handling_
  -- * Throwing
  , throwing
  ) where

import Control.Lens
import Control.Monad.Error
import Data.Monoid

-- | Helper function to provide conditional catch behavior.
catchJust :: MonadError e m => (e -> Maybe t) -> m a -> (t -> m a) -> m a
catchJust f m k = catchError m $ \ e -> case f e of
  Nothing -> throwError e
  Just x  -> k x

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
catching :: MonadError e m => Getting (Endo (Maybe a)) e t a b -> m r -> (a -> m r) -> m r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- | Catch exceptions that match a given 'Prism' (or any 'Getter'), discarding
-- the information about the match. This is particuarly useful when you have
-- a @'Prism'' e ()@ where the result of the prism or fold isn't
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
catching_ :: MonadError e m => Getting (Endo (Maybe a)) e t a b -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

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
handling :: MonadError e m => Getting (Endo (Maybe a)) e t a b -> (a -> m r) -> m r -> m r
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
handling_ :: MonadError e m => Getting (Endo (Maybe a)) e t a b -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

-- |
-- Throw an exception described by a 'Prism'.
-- @'throwing' l ≡ 'reviews' l 'throwError'@
--
-- @
-- 'throwing' :: 'MonadError' e m => 'Prism'' e t -> t -> a
-- 'throwing' :: 'MonadError' e m => 'Iso'' e t   -> t -> a
-- @
throwing :: MonadError e m => AReview e e t t -> t -> m x
throwing l = reviews l throwError
{-# INLINE throwing #-}
