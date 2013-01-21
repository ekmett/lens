{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Simple
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Deprecated names for simple lenses, etc.
-----------------------------------------------------------------------------
module Control.Lens.Simple where

import Control.Lens.Reified
import Control.Lens.Setter
import Control.Lens.Type

------------------------------------------------------------------------------
-- Deprecated
------------------------------------------------------------------------------

-- | Deprecated. Use 'Lens''.
type SimpleLens s a = Lens s s a a
{-# DEPRECATED SimpleLens "use Lens'" #-}

-- | Deprecated. Use 'ReifiedLens''.
type SimpleReifiedLens s a = Lens s s a a
{-# DEPRECATED SimpleReifiedLens "use Lens'" #-}

-- | Deprecated. Use 'Traversal''.
type SimpleTraversal s a = Traversal s s a a
{-# DEPRECATED SimpleTraversal "use Traversal'" #-}

-- | Deprecated. Use 'ReifiedTraversal''.
type SimpleReifiedTraversal s a = ReifiedTraversal s s a a
{-# DEPRECATED SimpleReifiedTraversal "use ReifiedTraversal'" #-}

-- | Deprecated. Use 'IndexedTraversal''.
type SimpleIndexedTraversal i s a = IndexedTraversal i s s a a
{-# DEPRECATED SimpleIndexedTraversal "use IndexedTraversal'" #-}

-- | Deprecated. Use 'ReifiedIndexedTraversal''.
type SimpleReifiedIndexedTraversal i s a = ReifiedIndexedTraversal i s s a a
{-# DEPRECATED SimpleReifiedIndexedTraversal "use ReifiedIndexedTraversal'" #-}

-- | Deprecated. Use 'Setter''.
type SimpleSetter s a = Setter s s a a
{-# DEPRECATED SimpleSetter "use Setter'" #-}

-- | Deprecated. Use 'ReifiedSetter''.
type SimpleReifiedSetter s a = ReifiedSetter s s a a
{-# DEPRECATED SimpleReifiedSetter "use ReifiedSetter'" #-}

-- | Deprecated. Use 'IndexedSetter''.
type SimpleIndexedSetter i s a = IndexedSetter i s s a a
{-# DEPRECATED SimpleIndexedSetter "use IndexedSetter'" #-}

-- | Deprecated. Use 'ReifiedIndexedSetter''.
type SimpleReifiedIndexedSetter i s a = ReifiedIndexedSetter i s s a a
{-# DEPRECATED SimpleReifiedIndexedSetter "use ReifiedIndexedSetter'" #-}

-- | Deprecated. Use 'Iso''.
type SimpleIso s a = Iso s s a a
{-# DEPRECATED SimpleIso "use Iso'" #-}

-- | Deprecated. Use 'Prism''.
type SimplePrism s a = Prism s s a a
{-# DEPRECATED SimplePrism "use Prism'" #-}
