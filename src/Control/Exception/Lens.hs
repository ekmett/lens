{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Control.Exception
--
----------------------------------------------------------------------------
module Control.Exception.Lens
  ( exception
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens

-- |
-- Traverse the strongly typed 'Exception' contained in 'SomeException' where the type of your function matches
-- the desired 'Exception'.
--
-- @
-- exception :: ('Applicative' f, 'Exception' a, 'Exception' b)
--           => (a -> f b) -> 'SomeException' -> f 'SomeException'
-- @
exception :: (Exception a, Exception b) => Projection SomeException SomeException a b
exception = projecting toException $ \f e -> case fromException e of
  Just a  -> toException <$> f a
  Nothing -> pure e
{-# INLINE exception #-}
