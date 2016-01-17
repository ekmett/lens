{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Exit.Lens
-- Copyright   :  (C) 2013-16 Edward Kmett
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
#if __GLASGOW_HASKELL__ >= 710
  , pattern ExitFailure_
  , pattern ExitSuccess_
#endif
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import System.Exit

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Exit codes that a program can return with:
class AsExitCode t where
  _ExitCode :: Prism' t ExitCode

instance AsExitCode ExitCode where
  _ExitCode = id
  {-# INLINE _ExitCode #-}

instance AsExitCode SomeException where
  _ExitCode = exception
  {-# INLINE _ExitCode #-}

-- | indicates successful termination;
--
-- @
-- '_ExitSuccess' :: 'Prism'' 'ExitCode'      ()
-- '_ExitSuccess' :: 'Prism'' 'SomeException' ()
-- @
_ExitSuccess :: AsExitCode t => Prism' t ()
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
_ExitFailure :: AsExitCode t => Prism' t Int
_ExitFailure = _ExitCode . dimap seta (either id id) . right' . rmap (fmap ExitFailure) where
  seta (ExitFailure i) = Right i
  seta t               = Left  (pure t)
{-# INLINE _ExitFailure #-}

#if __GLASGOW_HASKELL__ >= 710
pattern ExitSuccess_ <- (has _ExitSuccess -> True) where
  ExitSuccess_ = review _ExitSuccess ()

pattern ExitFailure_ a <- (preview _ExitFailure -> Just a) where
  ExitFailure_ a = review _ExitFailure a
#endif
