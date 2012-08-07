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
-- This package provides lens families, setters, getters, traversals,
-- isomorphisms, and folds that can all be composed automatically with
-- each other (and other lenses from other van Laarhoven lens libraries)
-- using @(.)@ from Prelude, while reducing the complexity of the API.
--
-- For a longer description and motivation of why you should care about lens families,
-- see <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Note: If you merely want your library to /provide/ lenses you may not
-- have to actually import /any/ lens library. For, say, a
-- @'Simple' 'Lens' Bar Foo@, just export a function with the signature:
--
-- > foo :: Functor f => (Foo -> f Foo) -> Bar -> f Bar
--
-- and then you can compose it with other lenses with @(.)@ without needing
-- anything from this library at all.
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-- > import Control.Lens
-- > data Foo a = Foo { _fooArgs :: [String], _fooValue :: a }
-- > makeLenses ''Foo
--
-- This defines the following lenses:
--
-- > fooArgs :: Simple Lens (Foo a) [String]
-- > fooValue :: Lens (Foo a) (Foo b) a b
--
-- The combinators here have unusually specific type signatures, so for
-- particularly tricky ones, I've tried to list the simpler type signatures
-- you might want to pretend the combinators have.
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
import Control.Lens.Representable
import Control.Lens.TH
