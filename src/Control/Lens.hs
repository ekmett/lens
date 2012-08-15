{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-- @
-- import Control.Lens
-- data Foo a = Foo { _fooArgs :: ['String'], _fooValue :: a }
-- 'makeLenses' ''Foo
-- @
--
-- This defines the following lenses:
--
-- @
-- fooArgs :: 'Simple' 'Lens' (Foo a) ['String']
-- fooValue :: 'Lens' (Foo a) (Foo b) a b
-- @
--
-- You can then access the value with ('^.') and set the value of the field
-- with ('.~') and can use almost any other combinator that is re-exported
-- here on those fields.
--
-- The combinators here have unusually specific type signatures, so for
-- particularly tricky ones, the simpler type signatures you might want to
-- pretend the combinators have are specified as well.
--
-- More information on how to use lenses is available on the lens wiki:
--
-- <http://github.com/ekmett/lens/wiki>
--
-- <<http://github.com/ekmett/lens/wiki/images/Hierarchy-2.3.png>>
----------------------------------------------------------------------------
module Control.Lens
  ( module Control.Lens.Type
  , module Control.Lens.Traversal
  , module Control.Lens.Getter
  , module Control.Lens.Setter
  , module Control.Lens.Action
  , module Control.Lens.Fold
  , module Control.Lens.Iso
  , module Control.Lens.Indexed
  , module Control.Lens.IndexedFold
  , module Control.Lens.IndexedGetter
  , module Control.Lens.IndexedLens
  , module Control.Lens.IndexedTraversal
  , module Control.Lens.IndexedSetter
  , module Control.Lens.Representable
  , module Control.Lens.TH
  ) where

import Control.Lens.Type
import Control.Lens.Traversal
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Action
import Control.Lens.Fold
import Control.Lens.Iso
import Control.Lens.Indexed
import Control.Lens.IndexedFold
import Control.Lens.IndexedGetter
import Control.Lens.IndexedLens
import Control.Lens.IndexedTraversal
import Control.Lens.IndexedSetter
import Control.Lens.Representable
import Control.Lens.TH
