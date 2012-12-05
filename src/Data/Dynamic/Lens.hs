{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Dynamic.Lens
  ( dynamic
  ) where

import Control.Lens
import Data.Dynamic

-- |
-- Traverse the typed value contained in a 'Dynamic' where the type required by your function matches that
-- of the contents of the 'Dynamic'.
dynamic :: Typeable a => Simple Prism Dynamic a
dynamic = prism toDyn $ \e -> maybe (Left e) Right (fromDynamic e)
{-# INLINE dynamic #-}
