{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-- @
-- import Control.Lens
-- 
-- data FooBar a
--   = Foo { _x :: ['Int'], _y :: a }
--   | Bar { _x :: ['Int'] }
-- 'makeLenses' ''FooBar
-- @
--
-- This defines the following lenses:
--
-- @
-- x :: 'Lens'' (FooBar a) ['Int']
-- y :: 'Traversal' (FooBar a) (FooBar b) a b
-- @
--
-- You can then access the value of @_x@ with ('^.'), the value of @_y@ –
-- with ('^?') or ('^?!') (since it can fail), set the values with ('.~'),
-- modify them with ('%~'), and use almost any other combinator that is
-- re-exported here on those fields.
--
-- The combinators here have unusually specific type signatures, so for
-- particularly tricky ones, the simpler type signatures you might want to
-- pretend the combinators have are specified as well.
--
-- More information on how to use lenses is available on the lens wiki:
--
-- <http://github.com/ekmett/lens/wiki>
--
-- <<Hierarchy.png>>
----------------------------------------------------------------------------
module Control.Lens
  ( module Control.Lens.At
  , module Control.Lens.Cons
  , module Control.Lens.Each
  , module Control.Lens.Empty
  , module Control.Lens.Equality
  , module Control.Lens.Fold
  , module Control.Lens.Getter
  , module Control.Lens.Indexed
  , module Control.Lens.Iso
  , module Control.Lens.Lens
  , module Control.Lens.Level
  , module Control.Lens.Plated
  , module Control.Lens.Prism
  , module Control.Lens.Reified
  , module Control.Lens.Review
  , module Control.Lens.Setter
#ifndef DISABLE_TEMPLATE_HASKELL
  , module Control.Lens.TH
#endif
  , module Control.Lens.Traversal
  , module Control.Lens.Tuple
  , module Control.Lens.Type
  , module Control.Lens.Wrapped
  , module Control.Lens.Zoom
  ) where

import Control.Lens.At
import Control.Lens.Cons
import Control.Lens.Each
import Control.Lens.Empty
import Control.Lens.Equality
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Level
import Control.Lens.Plated
import Control.Lens.Prism
import Control.Lens.Reified
import Control.Lens.Review
import Control.Lens.Setter
#ifndef DISABLE_TEMPLATE_HASKELL
import Control.Lens.TH
#endif
import Control.Lens.Traversal
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped
import Control.Lens.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif
