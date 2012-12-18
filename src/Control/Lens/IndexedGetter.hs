{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedGetter
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  rank 2 types, MPTCs
--
----------------------------------------------------------------------------
module Control.Lens.IndexedGetter
  (
  -- * Indexed Getters
    IndexedGetter
  , IndexedGetting
  , ReifiedIndexedGetter(..)
  -- * Indexed Getter Combinators
  , iview, iviews
  , iuse, iuses
  ) where

import Control.Lens.Indexed
import Control.Lens.Internal
import Control.Lens.Internal.Combinators
import Control.Lens.Classes
import Control.Monad.Reader
import Control.Monad.State

------------------------------------------------------------------------------
-- Indexed Getters
------------------------------------------------------------------------------

-- | Every 'IndexedGetter' is a valid 'Control.Lens.IndexedFold.IndexedFold' and 'Getter'.
type IndexedGetter i s a = forall k f. (Indexable i k, Gettable f) => k (a -> f a) (s -> f s)

-- | Used to consume an 'Control.Lens.IndexedFold.IndexedFold'.
type IndexedGetting i m s t a b = Indexed i (a -> Accessor m b) (s -> Accessor m t)

-- | Useful for storage.
newtype ReifiedIndexedGetter i s a = ReifyIndexedGetter { reflectIndexedGetter :: IndexedGetter i s a }

-- | View the index and value of an 'IndexedGetter' into the current environment as a pair.
--
-- When applied to an 'IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iview :: MonadReader s m => IndexedGetting i (i,a) s t a b -> m (i,a)
iview l = asks (runAccessor # (withIndex l (\i -> Accessor # (,) i)))
{-# INLINE iview #-}

-- | View a function of the index and value of an 'IndexedGetter' into the current environment
--
-- When applied to an 'IndexedFold' the result will be a monoidal summary instead of a single answer.
--
-- @'iviews' ≡ 'Control.Lens.IndexedFold.ifoldMapOf'@
iviews :: MonadReader s m => IndexedGetting i r s t a b -> (i -> a -> r) -> m r
iviews l f = asks (runAccessor # withIndex l (\i -> Accessor # f i))
{-# INLINE iviews #-}

-- | Use the index and value of an 'IndexedGetter' into the current state as a pair.
--
-- When applied to an 'IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iuse :: MonadState s m => IndexedGetting i (i,a) s t a b -> m (i,a)
iuse l = gets (runAccessor # withIndex l (\i -> Accessor # (,) i))
{-# INLINE iuse #-}

-- | Use a function of the index and value of an 'IndexedGetter' into the current state.
--
-- When applied to an 'IndexedFold' the result will be a monoidal summary instead of a single answer.
iuses :: MonadState s m => IndexedGetting i r s t a b -> (i -> a -> r) -> m r
iuses l f = gets (runAccessor # (withIndex l (\i -> Accessor # f i)))
{-# INLINE iuses #-}
