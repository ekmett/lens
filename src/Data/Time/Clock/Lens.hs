{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Clock.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  LiberalTypeSynonyms
--
-- Provides fairly ad hoc overloading to access different notions of 'Time'.
--
----------------------------------------------------------------------------
module Data.Time.Clock.Lens
  ( posix
  , tai
  ) where

import Control.Applicative
import Control.Lens
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.TAI

-- | POSIX time
posix :: Simple Lens UTCTime POSIXTime
posix = iso posixSecondsToUTCTime utcTimeToPOSIXSeconds

-- | TAI absolute time
absoluteTime :: LeapSecondTable -> Simple Lens UTCTime AbsoluteTime
absoluteTime t = iso (utcToTAITime t) (taiToUTCTime t)
