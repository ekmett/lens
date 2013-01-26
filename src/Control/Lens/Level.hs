{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Level
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- This module provides combinators for breadth-first searching within
-- arbitrary traversals.
----------------------------------------------------------------------------
module Control.Lens.Level
  ( Level
  , levels
  , ilevels
  ) where

import Control.Applicative
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Level
import Control.Lens.Traversal
import Data.Profunctor.Unsafe

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Char

levelIns :: Bazaar (->) a b t -> [Level () a]
levelIns = go 0 . (runAccessor #. bazaar (Accessor #. deepening ())) where
  go k z = k `seq` runDeepening z k $ \ xs b ->
    xs : if b then (go $! k + 1) z else []
{-# INLINE levelIns #-}

levelOuts :: Bazaar (->) a b t -> [Level j b] -> t
levelOuts bz = runFlows $ runBazaar bz $ \ _ -> Flows $ \t -> case t of
  One _ a : _ -> a
  _           -> error "levelOuts: wrong shape"
{-# INLINE levelOuts #-}

-- | This provides a breadth-first 'Traversal' of the individual 'levels' of any other 'Traversal'
-- via iterative deepening depth-first search. The levels are returned to you in a compressed format.
--
-- This can permit us to extract the 'levels' directly:
--
-- >>> ["hello","world"]^..levels (traverse.traverse)
-- [Zero,Zero,One () 'h',Two 0 (One () 'e') (One () 'w'),Two 0 (One () 'l') (One () 'o'),Two 0 (One () 'l') (One () 'r'),Two 0 (One () 'o') (One () 'l'),One () 'd']
--
-- But we can also traverse them in turn:
--
-- >>> ["hello","world"]^..levels (traverse.traverse).traverse
-- "hewlolrold"
--
-- We can use this to traverse to a fixed depth in the tree of ('<*>') used in the 'Traversal':
--
-- >>> ["hello","world"] & taking 4 (levels (traverse.traverse)).traverse %~ toUpper
-- ["HEllo","World"]
--
-- Or we can use it to traverse the first @n@ elements in found in that 'Traversal' regardless of the depth
-- at which they were found.
--
-- >>> ["hello","world"] & taking 4 (levels (traverse.traverse).traverse) %~ toUpper
-- ["HELlo","World"]
--
-- The resulting 'Traversal' of the 'levels' which is indexed by the depth of each 'Level'.
--
-- >>> ["dog","cat"]^@..levels (traverse.traverse) <. traverse
-- [(2,'d'),(3,'o'),(3,'c'),(4,'g'),(4,'a'),(5,'t')]
--
-- /Note:/ Internally this is implemented by using an illegal 'Applicative', as it extracts information
-- in an order that violates the 'Applicative' laws.
levels :: ATraversal s t a b -> IndexedTraversal Int s t (Level () a) (Level () b)
levels l f s = levelOuts bz <$> traversed f (levelIns bz) where
  bz = l sell s
{-# INLINE levels #-}

ilevelIns :: Bazaar (Indexed i) a b t -> [Level i a]
ilevelIns = go 0 . (runAccessor #. bazaar (Indexed $ \ i -> Accessor #. deepening i)) where
  go k z = k `seq` runDeepening z k $ \ xs b ->
    xs : if b then (go $! k + 1) z else []
{-# INLINE ilevelIns #-}

ilevelOuts :: Bazaar (Indexed i) a b t -> [Level j b] -> t
ilevelOuts bz = runFlows $ runBazaar bz $ Indexed $ \ _ _ -> Flows $ \t -> case t of
  One _ a : _ -> a
  _           -> error "ilevelOuts: wrong shape"
{-# INLINE ilevelOuts #-}

-- | This provides a breadth-first 'Traversal' of the individual levels of any other 'Traversal'
-- via iterative deepening depth-first search. The levels are returned to you in a compressed format.
--
-- This is similar to 'levels', but retains the index of the original 'IndexedTraversal', so you can
-- access it when traversing the levels later on.
--
-- >>> ["dog","cat"]^@..ilevels (traversed<.>traversed).itraversed
-- [((0,0),'d'),((0,1),'o'),((1,0),'c'),((0,2),'g'),((1,1),'a'),((1,2),'t')]
--
-- The resulting 'Traversal' of the levels which is indexed by the depth of each 'Level'.
--
-- >>> ["dog","cat"]^@..ilevels (traversed<.>traversed)<.>itraversed
-- [((2,(0,0)),'d'),((3,(0,1)),'o'),((3,(1,0)),'c'),((4,(0,2)),'g'),((4,(1,1)),'a'),((5,(1,2)),'t')]
--
-- /Note:/ Internally this is implemented by using an illegal 'Applicative', as it extracts information
-- in an order that violates the 'Applicative' laws.
ilevels :: AnIndexedTraversal i s t a b -> IndexedTraversal Int s t (Level i a) (Level j b)
ilevels l f s = ilevelOuts bz <$> traversed f (ilevelIns bz) where
  bz = l sell s
{-# INLINE ilevels #-}
