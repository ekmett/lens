{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Note: @GHC.Generics@ exports a number of names that collide with @Control.Lens@.
--
-- You can use hiding or imports to mitigate this to an extent, and the following imports,
-- represent a fair compromise for user code:
--
-- > import Control.Lens hiding (Rep)
-- > import GHC.Generics hiding (from, to)
--
-- You can use 'generic' to replace 'GHC.Generics.from' and 'GHC.Generics.to' from @GHC.Generics@,
-- and probably won't be explicitly referencing 'Control.Lens.Representable.Rep' from @Control.Lens@
-- in code that uses generics.
--
-- This module provides compatibility with older GHC versions by using the
-- <http://hackage.haskell.org/package/generic-deriving generic-deriving>
-- package.
----------------------------------------------------------------------------
module GHC.Generics.Lens
  ( module Generics.Deriving.Lens
  ) where

import Generics.Deriving.Lens
