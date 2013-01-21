{-# LANGUAGE Rank2Types #-}
------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Reified
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
------------------------------------------------------------------------------
module Control.Lens.Reified where

import Control.Lens.Type

------------------------------------------------------------------------------
-- Reifying
------------------------------------------------------------------------------

-- | Reify a 'Lens' so it can be stored safely in a container.
newtype ReifiedLens s t a b = ReifyLens { reflectLens :: Lens s t a b }

-- | @
-- type 'ReifiedLens'' = 'Simple' 'ReifiedLens'
-- @
type ReifiedLens' s a = ReifiedLens s s a a

-- | Reify an 'IndexedLens' so it can be stored safely in a container.
newtype ReifiedIndexedLens i s t a b = ReifyIndexedLens { reflectIndexedLens :: IndexedLens i s t a b }

-- | @
-- type 'ReifiedIndexedLens'' i = 'Simple' ('ReifiedIndexedLens' i)
-- @
type ReifiedIndexedLens' i s a = ReifiedIndexedLens i s s a a

-- | Reify an 'IndexedTraversal' so it can be stored safely in a container.
newtype ReifiedIndexedTraversal i s t a b = ReifyIndexedTraversal { reflectIndexedTraversal :: IndexedTraversal i s t a b }

-- | @
-- type 'ReifiedIndexedTraversal'' i = 'Simple' ('ReifiedIndexedTraversal' i)
-- @
type ReifiedIndexedTraversal' i s a = ReifiedIndexedTraversal i s s a a

-- | A form of 'Traversal' that can be stored monomorphically in a container.
data ReifiedTraversal s t a b = ReifyTraversal { reflectTraversal :: Traversal s t a b }

-- | @
-- type 'ReifiedTraversal'' = 'Simple' 'ReifiedTraversal'
-- @
type ReifiedTraversal' s a = ReifiedTraversal s s a a

-- | Reify a 'Getter' so it can be stored safely in a container.
newtype ReifiedGetter s a = ReifyGetter { reflectGetter :: Getter s a }

-- | Reify an 'IndexedGetter' so it can be stored safely in a container.
newtype ReifiedIndexedGetter i s a = ReifyIndexedGetter { reflectIndexedGetter :: IndexedGetter i s a }

-- | Reify a 'Fold' so it can be stored safely in a container.
newtype ReifiedFold s a = ReifyFold { reflectFold :: Fold s a }

-- | Reify a 'Setter' so it can be stored safely in a container.
newtype ReifiedSetter s t a b = ReifySetter { reflectSetter :: Setter s t a b }

-- | @
-- type 'ReifiedSetter'' = 'Simple' 'ReifiedSetter'
-- @
type ReifiedSetter' s a = ReifiedSetter s s a a

-- | Reify an 'IndexedSetter' so it can be stored safely in a container.
newtype ReifiedIndexedSetter i s t a b =
  ReifyIndexedSetter { reflectIndexedSetter :: IndexedSetter i s t a b }

-- | @
-- type 'ReifiedIndexedSetter'' i = 'Simple' ('ReifiedIndexedSetter' i)
-- @
type ReifiedIndexedSetter' i s a = ReifiedIndexedSetter i s s a a

-- | Reify an 'Iso' so it can be stored safely in a container.
newtype ReifiedIso s t a b = ReifyIso { reflectIso :: Iso s t a b }

-- | @
-- type 'ReifiedIso'' = 'Simple' 'ReifiedIso'
-- @
type ReifiedIso' s a = ReifiedIso s s a a

-- | Reify a 'Prism' so it can be stored safely in a container.
newtype ReifiedPrism s t a b = ReifyPrism { reflectPrism :: Prism s t a b }

-- | @
-- type 'ReifiedPrism'' = 'Simple' 'ReifiedPrism'
-- @
type ReifiedPrism' s a = ReifiedPrism s s a a
