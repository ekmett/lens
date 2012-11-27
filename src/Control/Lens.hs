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
-- <<http://github.com/ekmett/lens/wiki/images/Hierarchy-3.6.png>>
----------------------------------------------------------------------------
module Control.Lens (module Exports) where

import Control.Lens.Action           as Exports
import Control.Lens.Combinators      as Exports
import Control.Lens.Fold             as Exports
import Control.Lens.Getter           as Exports
import Control.Lens.Indexed          as Exports
import Control.Lens.IndexedFold      as Exports
import Control.Lens.IndexedGetter    as Exports
import Control.Lens.IndexedLens      as Exports
import Control.Lens.IndexedSetter    as Exports
import Control.Lens.IndexedTraversal as Exports
import Control.Lens.Iso              as Exports
import Control.Lens.Loupe            as Exports
import Control.Lens.Plated           as Exports
import Control.Lens.Projection       as Exports
import Control.Lens.Representable    as Exports
import Control.Lens.Setter           as Exports
#ifndef DISABLE_TEMPLATE_HASKELL
import Control.Lens.TH               as Exports
#endif
import Control.Lens.Traversal        as Exports
import Control.Lens.Tuple            as Exports
import Control.Lens.Type             as Exports
import Control.Lens.WithIndex        as Exports
import Control.Lens.Zipper           as Exports
import Control.Lens.Zoom             as Exports
