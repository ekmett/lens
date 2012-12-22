{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Simple
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.Simple where

import Control.Lens.Reified
import Control.Lens.Setter
import Control.Lens.Type

------------------------------------------------------------------------------
-- Deprecated
------------------------------------------------------------------------------

type SimpleLens s a = Lens s s a a
{-# DEPRECATED SimpleLens "use Lens'" #-}

type SimpleReifiedLens s a = Lens s s a a
{-# DEPRECATED SimpleReifiedLens "use Lens'" #-}

type SimpleReifiedTraversal s a = ReifiedTraversal s s a a
{-# DEPRECATED SimpleReifiedTraversal "use ReifiedTraversal'" #-}

type SimpleTraversal s a = Traversal s s a a
{-# DEPRECATED SimpleTraversal "use Traversal'" #-}

type SimpleReifiedIndexedTraversal i s a = ReifiedIndexedTraversal i s s a a
{-# DEPRECATED SimpleReifiedIndexedTraversal "use ReifiedIndexedTraversal'" #-}

type SimpleIndexedTraversal i s a = IndexedTraversal i s s a a
{-# DEPRECATED SimpleIndexedTraversal "use IndexedTraversal'" #-}

type SimpleReifiedSetter s a = ReifiedSetter s s a a
{-# DEPRECATED SimpleReifiedSetter "use ReifiedSetter'" #-}

type SimpleSetter s a = Setter s s a a
{-# DEPRECATED SimpleSetter "use Setter'" #-}

type Setting s a = ASetter s s a a
{-# DEPRECATED Setting "use ASetter" #-}

type SimpleSetting s a = ASetter s s a a
{-# DEPRECATED SimpleSetting "use ASetter'" #-}

type SimpleReifiedIndexedSetter i s a = ReifiedIndexedSetter i s s a a
{-# DEPRECATED SimpleReifiedIndexedSetter "use ReifiedIndexedSetter'" #-}

type SimpleIndexedSetter i s a = IndexedSetter i s s a a
{-# DEPRECATED SimpleIndexedSetter "use IndexedSetter'" #-}

type SimpleIso s a = Iso s s a a
{-# DEPRECATED SimpleIso "use Iso'" #-}

type SimplePrism s a = Prism s s a a
{-# DEPRECATED SimplePrism "use Prism'" #-}
