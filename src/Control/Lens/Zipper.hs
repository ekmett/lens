-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Zipper
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides a 'Zipper' with fairly strong type checking guarantees.
--
-- The code here is inspired by Brandon Simmons' @zippo@ package, but uses
-- a slightly different approach to represent the 'Zipper' that makes the whole thing
-- look like his breadcrumb trail, and can move side-to-side through traversals.
--
-- Some examples types:
--
-- [@'Top' ':>' a@] represents a trivial 'Zipper' with its focus at the root.
--
-- [@'Top' ':>' 'Data.Tree.Tree' a ':>' a@] represents a zipper that starts with a
--   'Data.Tree.Tree' and descends in a single step to values of type @a@.
--
-- [@'Top' ':>' 'Data.Tree.Tree' a ':>' 'Data.Tree.Tree' a ':>' 'Data.Tree.Tree' a@] represents a 'Zipper' into a
--   'Data.Tree.Tree' with an intermediate bookmarked 'Data.Tree.Tree',
--   focusing in yet another 'Data.Tree.Tree'.
--
-- Since individual levels of a zipper are managed by an arbitrary 'Traversal',
-- you can move left and right through the 'Traversal' selecting neighboring elements.
--
-- >>> zipper ("hello","world") & down _1 & fromWithin traverse & focus .~ 'J' & farthest right & focus .~ 'y' & rezip
-- ("Jelly","world")
--
-- This is particularly powerful when compiled with 'Control.Lens.Plated.plate',
-- 'Data.Data.Lens.uniplate' or 'Data.Data.Lens.biplate' for walking down into
-- self-similar children in syntax trees and other structures.
-----------------------------------------------------------------------------
module Control.Lens.Zipper
  (
  -- * Zippers
    Top()
  , (:>)()
  , zipper
  -- ** Focusing
  , focus
  -- ** Horizontal Movement
  , up
  , down
  , within
  -- ** Lateral Movement
  , left
  , right
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
  -- ** Closing the zipper
  , rezip
  , Zipped
  , Zipper()
  -- ** Recording
  , Tape()
  , saveTape
  , restoreTape
  , restoreNearTape
  -- ** Unsafe Movement
  , fromWithin
  , unsafelyRestoreTape
  ) where

import Control.Lens.Internal.Zipper

-- $setup
-- >>> :m + Control.Lens
