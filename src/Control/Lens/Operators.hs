-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Operators
-- Copyright   :  (C) 2012-16 Edward Kmett
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
  ( -- output from scripts/operators -h
  -- * "Control.Lens.Cons"
    (<|)
  , (|>)
  , (<|~)
  , (<|=)
  , (<<|~)
  , (<<|=)
  , (<<<|~)
  , (<<<|=)
  , (|>~)
  , (|>=)
  , (<|>~)
  , (<|>=)
  , (<<|>~)
  , (<<|>=)
  -- * "Control.Lens.Fold"
  , (^..)
  , (^?)
  , (^?!)
  , (^@..)
  , (^@?)
  , (^@?!)
  -- * "Control.Lens.Getter"
  , (^.)
  , (^@.)
  -- * "Control.Lens.Indexed"
  , (<.)
  , (.>)
  , (<.>)
  -- * "Control.Lens.Lens"
  , (%%~)
  , (%%=)
  , (&)
  , (&~)
  , (<&>)
  , (??)
  , (<%~)
  , (<+~)
  , (<-~)
  , (<*~)
  , (<//~)
  , (<^~)
  , (<^^~)
  , (<**~)
  , (<||~)
  , (<&&~)
  , (<<%~)
  , (<<.~)
  , (<<?~)
  , (<<+~)
  , (<<-~)
  , (<<*~)
  , (<<//~)
  , (<<^~)
  , (<<^^~)
  , (<<**~)
  , (<<||~)
  , (<<&&~)
  , (<<<>~)
  , (<<<>:~)
  , (<%=)
  , (<+=)
  , (<-=)
  , (<*=)
  , (<//=)
  , (<^=)
  , (<^^=)
  , (<**=)
  , (<||=)
  , (<&&=)
  , (<<%=)
  , (<<.=)
  , (<<?=)
  , (<<+=)
  , (<<-=)
  , (<<*=)
  , (<<//=)
  , (<<^=)
  , (<<^^=)
  , (<<**=)
  , (<<||=)
  , (<<&&=)
  , (<<<>=)
  , (<<<>:=)
  , (<<~)
  , (<<>~)
  , (<<>=)
  , (<<>:~)
  , (<<>:=)
  , (<%@~)
  , (<<%@~)
  , (%%@~)
  , (%%@=)
  , (<%@=)
  , (<<%@=)
  , (^#)
  , (#~)
  , (#%~)
  , (#%%~)
  , (#=)
  , (#%=)
  , (<#%~)
  , (<#%=)
  , (#%%=)
  , (<#~)
  , (<#=)
  -- * "Control.Lens.Plated"
  , (...)
  -- * "Control.Lens.Review"
  , (#)
  -- * "Control.Lens.Setter"
  , (%~)
  , (.~)
  , (?~)
  , (<.~)
  , (<?~)
  , (+~)
  , (*~)
  , (-~)
  , (//~)
  , (^~)
  , (^^~)
  , (**~)
  , (||~)
  , (&&~)
  , (.=)
  , (%=)
  , (?=)
  , (+=)
  , (-=)
  , (*=)
  , (//=)
  , (^=)
  , (^^=)
  , (**=)
  , (&&=)
  , (||=)
  , (<~)
  , (<.=)
  , (<?=)
  , (<>~)
  , (<>=)
  , (<>:~)
  , (<>:=)
  , (.@~)
  , (.@=)
  , (%@~)
  , (%@=)
  ) where

import Control.Lens
