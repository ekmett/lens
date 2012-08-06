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
-- > import Control.Lens.TH
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
  (
  -- * Lenses
    Lens
  , LensLike
  , Traversal
  , Simple
  , (%%~), (%%=)
  , lens

  -- ** Common Lenses
  , _1, _2
  , resultAt
  , element
  , elementOf

  -- * Isomorphisms
  , Isomorphic(..)
  , Isomorphism(..)
  , from
  , via

  , Iso
  , iso
  , isos
  , Overloaded

  -- * Setters
  , Setter
  , Setting
  , sets
  , mapped
  , adjust, mapOf
  , set
  , whisper
  , (.~), (%~), (+~), (-~), (*~), (//~), (||~), (&&~), (<>~)
  , (.=), (%=), (+=), (-=), (*=), (//=), (||=), (&&=), (<>=)

  -- * Getters
  , Getter
  , Getting
  , to
  , view, views
  , (^.), (^$)
  , use, uses
  , query, queries

  -- * Folds
  , Fold
  , folds
  , folding
  , folded
  , unfolded
  , iterated
  , filtered
  -- , reversed
  , repeated
  , replicated
  , cycled
  , takingWhile
  , droppingWhile

  -- ** Getting and Folding
  , foldMapOf, foldOf
  , foldrOf, foldlOf
  , toListOf
  , anyOf, allOf
  , andOf, orOf
  , productOf, sumOf
  , traverseOf_, forOf_, sequenceAOf_
  , mapMOf_, forMOf_, sequenceOf_
  , asumOf, msumOf
  , concatMapOf, concatOf
  , elemOf, notElemOf
  , lengthOf
  , nullOf
  , headOf, lastOf
  , maximumOf, minimumOf
  , maximumByOf, minimumByOf
  , findOf
  , foldrOf', foldlOf'
  , foldr1Of, foldl1Of
  , foldrMOf, foldlMOf

  -- * Traversing and Lensing
  , Focus(..)
  , traverseOf, forOf, sequenceAOf
  , mapMOf, forMOf, sequenceOf
  , transposeOf
  , mapAccumLOf, mapAccumROf
  , scanr1Of, scanl1Of

  -- * Common Traversals
  , Traversable(traverse)
  , traverseNothing

  -- * Transforming Traversals
  , backwards

  -- * Cloning Lenses
  , clone
  , merged
  , bothLenses

  -- ** Common Isomorphisms
  , identity
  , _const

  -- * Indexed Functions
  , Index(..)
  , Indexed(..)

  -- * Indexed Folds
  , IndexedFold
  , foldMapWithIndexOf
  , foldrWithIndexOf

  -- * Indexed Traversals
  , IndexedTraversal
  , traverseWithIndexOf
  , mapMWithIndexOf

  -- * Indexed Setter
  , IndexedSetter
  , mapWithIndexOf
  , (%@)

  -- * Simple
  , SimpleSetter
  , SimpleSetting
  , SimpleIndexedTraversal
  , SimpleIndexedSetter
  , SimpleIso
  , SimpleOverloaded
  , SimpleLens
  , SimpleTraversal
  , SimpleLensLike
  ) where

import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.Iso
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type

