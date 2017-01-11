-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- One of most commonly-asked questions about this package is whether
-- it provides lenses for working with 'Data.Map.Map'. It does, but their uses
-- are perhaps obscured by their genericity. This module exists to provide
-- documentation for them.
--
-- 'Data.Map.Map' is an instance of 'Control.Lens.At.At', so we have a lenses
-- on values at keys:
--
-- >>> Map.fromList [(1, "world")] ^.at 1
-- Just "world"
--
-- >>> at 1 .~ Just "world" $ Map.empty
-- fromList [(1,"world")]
--
-- >>> at 0 ?~ "hello" $ Map.empty
-- fromList [(0,"hello")]
--
-- We can traverse, fold over, and map over key-value pairs in a
-- 'Data.Map.Map', thanks to its 'Control.Lens.Indexed.TraversableWithIndex',
-- 'Control.Lens.Indexed.FoldableWithIndex', and
-- 'Control.Lens.Indexed.FunctorWithIndex' instances.
--
-- >>> imap const $ Map.fromList [(1, "Venus")]
-- fromList [(1,1)]
--
-- >>> ifoldMap (\i _ -> Sum i) $ Map.fromList [(2, "Earth"), (3, "Mars")]
-- Sum {getSum = 5}
--
-- >>> itraverse_ (curry print) $ Map.fromList [(4, "Jupiter")]
-- (4,"Jupiter")
--
-- >>> itoList $ Map.fromList [(5, "Saturn")]
-- [(5,"Saturn")]
--
-- A related class, 'Control.Lens.At.Ixed', allows us to use
-- 'Control.Lens.At.ix' to traverse a value at a particular key.
--
-- >>> ix 2 %~ ("New " ++) $ Map.fromList [(2, "Earth")]
-- fromList [(2,"New Earth")]
--
-- >>> preview (ix 8) $ Map.empty
-- Nothing
--
-- Additionally, 'Data.Map.Map' has 'Control.Lens.Traversal.TraverseMin' and
-- 'Control.Lens.Traversal.TraverseMax' instances, which let us traverse over
-- the value at the least and greatest keys, respectively.
--
-- >>> preview traverseMin $ Map.fromList [(5, "Saturn"), (6, "Uranus")]
-- Just "Saturn"
--
-- >>> preview traverseMax $ Map.fromList [(5, "Saturn"), (6, "Uranus")]
-- Just "Uranus"
--
-----------------------------------------------------------------------------
module Data.Map.Lens
  ( toMapOf
  ) where

import Control.Lens.Getter ( IndexedGetting, iviews )
import qualified Data.Map as Map

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Monoid
-- >>> :set -XNoOverloadedStrings

-- | Construct a map from a 'IndexedGetter', 'Control.Lens.Fold.IndexedFold', 'Control.Lens.Traversal.IndexedTraversal' or 'Control.Lens.Lens.IndexedLens'
--
-- The construction is left-biased (see 'Data.Map.Lazy.union'), i.e. the first
-- occurences of keys in the fold or traversal order are preferred.
--
-- >>> toMapOf folded ["hello", "world"]
-- fromList [(0,"hello"),(1,"world")]
--
-- >>> toMapOf (folded . ifolded) [('a',"alpha"),('b', "beta")]
-- fromList [('a',"alpha"),('b',"beta")]
--
-- >>> toMapOf (folded <.> folded) ["foo", "bar"]
-- fromList [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
-- >>> toMapOf ifolded $ Map.fromList [('a', "hello"), ('b', "world")]
-- fromList [('a',"hello"),('b',"world")]
--
-- @
-- 'toMapOf' ::          'IndexedGetter' i s a     -> s -> 'Map.Map' i a
-- 'toMapOf' :: 'Ord' i => 'IndexedFold' i s a       -> s -> 'Map.Map' i a
-- 'toMapOf' ::          'IndexedLens'' i s a      -> s -> 'Map.Map' i a
-- 'toMapOf' :: 'Ord' i => 'IndexedTraversal'' i s a -> s -> 'Map.Map' i a
-- @
toMapOf :: IndexedGetting i (Map.Map i a) s a -> s -> Map.Map i a
toMapOf l = iviews l Map.singleton
