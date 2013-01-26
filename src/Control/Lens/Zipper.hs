-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Zipper
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides a 'Zipper' with fairly strong type checking guarantees.
--
-- The code here is inspired by Brandon Simmons' @zippo@ package, but uses
-- a different approach to represent the 'Zipper' that makes the whole thing
-- look like his breadcrumb trail, and can move side-to-side through
-- traversals.
--
-- Some examples types:
--
-- [@'Top' ':>>' a@] represents a trivial 'Zipper' with its focus at the root.
--
-- [@'Top' ':>>' 'Data.Tree.Tree' a ':>>' a@] represents a 'Zipper' that starts with a
--   'Data.Tree.Tree' and descends in a single step to values of type @a@.
--
-- [@'Top' ':>>' 'Data.Tree.Tree' a ':>>' 'Data.Tree.Tree' a ':>>' 'Data.Tree.Tree' a@] represents a 'Zipper' into a
--   'Data.Tree.Tree' with an intermediate bookmarked 'Data.Tree.Tree',
--   focusing in yet another 'Data.Tree.Tree'.
--
-- Since individual levels of a 'Zipper' are managed by an arbitrary
-- 'Control.Lens.Type.IndexedTraversal', you can move left and right through
-- the 'Control.Lens.Type.IndexedTraversal' selecting neighboring elements.
--
-- >>> zipper ("hello","world") & downward _1 & fromWithin traverse & focus .~ 'J' & rightmost & focus .~ 'y' & rezip
-- ("Jelly","world")
--
-- This is particularly powerful when compiled with 'Control.Lens.Plated.plate',
-- 'Data.Data.Lens.uniplate' or 'Data.Data.Lens.biplate' for walking down into
-- self-similar children in syntax trees and other structures.
--
-- Given keys in ascending order you can jump directly to a given key with
-- 'moveTo'. When used with traversals for balanced
-- tree-like structures such as an 'Data.IntMap.IntMap' or 'Data.Map.Map',
-- searching for a key with 'moveTo' can be done in logarithmic time.
-----------------------------------------------------------------------------
module Control.Lens.Zipper
  (
  -- * Zippers
    Top()
  , (:>)()
  , (:>>)()
  , (:@)()
  , Zipper
  , zipper
  -- ** Focusing
  , focus
  , focusedContext
  -- ** Vertical Movement
  , upward
  , downward, idownward
  , within, iwithin
  , withins, iwithins
  -- ** Lateral Movement
  , leftward, rightward
  , leftmost, rightmost
  -- ** Movement Combinators
  , tug
  , tugs
  , jerks
  , farthest
  -- ** Absolute Positioning
  , tooth
  , teeth
  , jerkTo
  , tugTo
  , moveTo
  , moveToward
  -- ** Closing the zipper
  , rezip
  , Zipped
  , Zipping()
  -- ** Recording
  , Tape()
  , saveTape
  , restoreTape
  , restoreNearTape
  -- ** Unsafe Movement
  , fromWithin
  , ifromWithin
  , unsafelyRestoreTape
  ) where

import Control.Lens.Internal.Zipper

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
