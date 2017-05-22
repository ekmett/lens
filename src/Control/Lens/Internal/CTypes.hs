-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.CTypes
-- Copyright   :  (C) 2012-2016 Edward Kmett, (C) 2017 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- In "Control.Lens.Wrapped", we need to muck around with the internals of the
-- newtypes in "Foreign.C.Types". Unfortunately, the exact types used varies
-- wildly from platform to platform, so trying to manage the imports necessary
-- to bring these types in scope can be unwieldy.
--
-- To make things easier, we use this module as a way to import everything
-- carte blanche that might be used internally in "Foreign.C.Types". For
-- now, this consists of all the exports from the "Data.Int" and "Data.Word"
-- modules, as well as the 'Ptr' type.
----------------------------------------------------------------------------
module Control.Lens.Internal.CTypes
  ( module Data.Int
  , Ptr
  , module Data.Word
  ) where

import Data.Int
import Data.Word
import Foreign.Ptr (Ptr)
