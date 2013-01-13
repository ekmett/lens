{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Exit.Lens
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Control.Exception
--
-- These prisms can be used with the combinators in "Control.Exception.Lens".
----------------------------------------------------------------------------
module System.Exit.Lens
  ( AsExitCode(..)
  , _ExitFailure
  , _ExitSuccess
  ) where

import Control.Applicative
import Control.Exception
import Control.Exception.Lens
import Control.Lens
import System.Exit

-- | Exit codes that a program can return with:
class AsExitCode p f t where
  -- |
  -- @
  -- '_ExitCode' :: 'Equality'' 'ExitCode'      'ExitCode'
  -- '_ExitCode' :: 'Prism''    'SomeException' 'ExitCode'
  -- @
  _ExitCode :: Overloaded' p f t ExitCode

instance AsExitCode p f ExitCode where
  _ExitCode = id
  {-# INLINE _ExitCode #-}

instance (Choice p, Applicative f) => AsExitCode p f SomeException where
  _ExitCode = exception
  {-# INLINE _ExitCode #-}

-- | indicates successful termination;
--
-- @
-- '_ExitSuccess' :: 'Prism'' 'ExitCode'      ()
-- '_ExitSuccess' :: 'Prism'' 'SomeException' ()
-- @
_ExitSuccess :: (AsExitCode p f t, Choice p, Applicative f) => Overloaded' p f t ()
_ExitSuccess = _ExitCode . dimap seta (either id id) . right' . rmap (ExitSuccess <$) where
  seta ExitSuccess = Right ()
  seta t           = Left  (pure t)
{-# INLINE _ExitSuccess #-}

-- | indicates program failure with an exit code. The exact interpretation of the code is operating-system dependent. In particular, some values may be prohibited (e.g. 0 on a POSIX-compliant system).
--
-- @
-- '_ExitFailure' :: 'Prism'' 'ExitCode'      'Int'
-- '_ExitFailure' :: 'Prism'' 'SomeException' 'Int'
-- @
_ExitFailure :: (AsExitCode p f t, Choice p, Applicative f) => Overloaded' p f t Int
_ExitFailure = _ExitCode . dimap seta (either id id) . right' . rmap (fmap ExitFailure) where
  seta (ExitFailure i) = Right i
  seta t               = Left  (pure t)
{-# INLINE _ExitFailure #-}
