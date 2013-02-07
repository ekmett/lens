-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Operators
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exists for users who like to work with qualified imports
-- but want access to the operators from Lens.
--
-- > import qualified Control.Lens as L
-- > import Control.Lens.Operators
----------------------------------------------------------------------------
module Control.Lens.Operators
  (
  -- * General Purpose
    (&), (<&>), (??)

  -- * Getting
  , (^.), (^@.)
  -- ** Loupes
  , (^#)
  -- ** with Effects
  , (^!), (^@!)
  , (^!!), (^@!!)
  , (^!?), (^@!?)
  -- ** from Folds
  , (^..), (^@..)
  , (^?), (^@?)
  , (^?!), (^@?!)

  -- * Reviewing
  , ( # )

  -- * Common Operators
  -- ** Setting
  , (.~) , (.=)
  , (<.~), (<.=)
  , (<<.~), (<<.=)
  --- *** Loupes
  , ( #~ ), ( #= )
  , (<#~), (<#=)
  -- *** Just
  , (?~), (?=)
  , (<?~), (<?=)

  -- ** Modifying
  , (%~), (%=)
  , (<%~), (<%=)
  , (<<%~), (<<%=)
  -- *** Loupes
  , ( #%~ ), ( #%= )
  , (<#%~), (<#%=)
  -- *** with Indices
  , (%@~), (%@=)
  , (<%@~), (<%@=)
  , (<<%@~), (<<%@=)

  -- ** Traversing
  , (%%~), (%%=)
   --- *** Loupes
  , ( #%%~ ), ( #%%= )
   --- *** with Indices
  , (%%@~), (%%@=)

  -- ** Addition
  , (+~), (+=), (<+~), (<+=)
  -- ** Subtraction
  , (-~), (-=), (<-~), (<-=)
  -- ** Multiplication
  , (*~), (*=), (<*~), (<*=)
  -- ** Division
  , (//~), (//=), (<//~), (<//=)
  -- ** Exponentiation
  , (^~), (^=), (<^~), (<^=)
  , (^^~), (^^=), (<^^~), (<^^=)
  , (**~), (**=), (<**~), (<**=)
  -- ** Logical Or
  , (||~), (||=), (<||~), (<||=)
  -- ** Logical And
  , (&&~), (&&=), (<&&~), (<&&=)
  -- ** Monoidal
  , (<>~), (<>=), (<<>~), (<<>=)

  -- * Composing Indices
  , (<.>), (<.), (.>)

  -- * Monadic Assignment
  , (<~), (<<~)

  -- * Zippers
  , (:>)(), (:>>)()

  -- * Cons and Snoc
  , (<|), (|>)
  ) where

import Control.Lens.Action
import Control.Lens.Combinators
import Control.Lens.Cons
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.Lens
import Control.Lens.Review
import Control.Lens.Setter
import Control.Lens.Zipper

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
