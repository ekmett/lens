-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Monoid.Lens
  (
  -- * Monoid Lens Operators
    (<>~), (<<>~)
  , (<>=), (<<>=)
  ) where

import Data.Monoid
import Control.Lens
import Control.Monad.State as State

-- $setup
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable

infixr 4 <>~, <<>~
infix 4 <>=, <<>=

-- | Modify the target of a monoidally valued by 'mappend'ing another value.
--
-- >>> both <>~ "!!!" $ ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('<>~') :: 'Monoid' a => 'Setter' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Iso' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Lens' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Traversal' s t a a -> a -> s -> t
-- @
(<>~) :: Monoid a => Setting s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by 'mappend'ing a value.
--
-- >>> execState (both <>= "!!!") ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Setter' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Iso' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Lens' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Traversal' s a -> a -> m ()
-- @
(<>=) :: (MonadState s m, Monoid a) => SimpleSetting s a -> a -> m ()
l <>= a = State.modify (l <>~ a)
{-# INLINE (<>=) #-}


-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' and
-- return the result
--
-- When you do not need the result of the operation, ('<>~') is more flexible.
(<<>~) :: Monoid m => LensLike ((,)m) s t m m -> m -> s -> (m, t)
l <<>~ m = l <%~ (`mappend` m)
{-# INLINE (<<>~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- When you do not need the result of the operation, ('<>=') is more flexible.
(<<>=) :: (MonadState s m, Monoid r) => SimpleLensLike ((,)r) s r -> r -> m r
l <<>= r = l <%= (`mappend` r)
{-# INLINE (<<>=) #-}
