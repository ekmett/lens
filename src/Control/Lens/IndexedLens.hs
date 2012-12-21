{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedLens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types, MPTCs, TFs, flexible
--
----------------------------------------------------------------------------
module Control.Lens.IndexedLens
  (
  -- * Indexed Lenses
    IndexedLens
  , IndexedLens'
  -- * Indexed Lens Combinators
  , (%%@~)
  , (<%@~)
  , (%%@=)
  , (<%@=)
  -- * Storing Indexed Lenses
  , ReifiedIndexedLens(..)
  , ReifiedIndexedLens'
  -- * Deprecated
  , SimpleIndexedLens
  , SimpleReifiedIndexedLens
  ) where

import Control.Lens.Classes
import Control.Lens.Indexed
import Control.Monad.State.Class as State

-- $setup
-- >>> import Control.Lens

infixr 4 %%@~, <%@~
infix  4 %%@=, <%@=

-- | Every 'IndexedLens' is a valid 'Lens' and a valid 'Control.Lens.IndexedTraversal.IndexedTraversal'.
type IndexedLens i s t a b = forall f k. (Indexable i k, Functor f) => k a (f b) -> s -> f t

-- | @type 'IndexedLens'' i = 'Simple' ('IndexedLens' i)@
type IndexedLens' i s a = IndexedLens i s s a a

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' and return a monoidal summary
-- along with the answer.
--
-- @l '<%~' f ≡ l '<%@~' 'const' f@
--
-- When you do not need access to the index then ('<%~') is more liberal in what it can accept.
--
-- If you do not need the intermediate result, you can use ('Control.Lens.Type.%@~') or even ('Control.Lens.Type.%~').
--
-- @
-- ('<%@~') ::             'IndexedLens' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- ('<%@~') :: 'Monoid' b => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- @
(<%@~) :: IndexedLensLike (Indexed i) ((,) b) s t a b -> (i -> a -> b) -> s -> (b, t)
l <%@~ f = l @~ \i a -> let b = f i a in (b, b)
{-# INLINE (<%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' and return a monoidal summary
-- of the supplementary results and the answer.
--
-- @('%%@~') ≡ 'withIndex'@
--
-- @
-- ('%%@~') :: 'Functor' f => 'IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- ('%%@~') :: 'Functor' f => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
--
-- In particular, it is often useful to think of this function as having one of these even more
-- restrictive type signatures
--
-- @
-- ('%%@~') ::             'IndexedLens' i s t a b      -> (i -> a -> (r, b)) -> s -> (r, t)
-- ('%%@~') :: 'Monoid' r => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> (r, b)) -> s -> (r, t)
-- @
(%%@~) :: IndexedLensLike (Indexed i) f s t a b -> (i -> a -> f b) -> s -> f t
(%%@~) = withIndex
{-# INLINE (%%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the supplementary results.
--
-- @l '%%@=' f ≡ 'state' (l '%%@~' f)@
--
-- @
-- ('%%@=') :: 'MonadState' s m                'IndexedLens' i s s a b      -> (i -> a -> (r, b)) -> s -> m r
-- ('%%@=') :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s s a b -> (i -> a -> (r, b)) -> s -> m r
-- @
(%%@=) :: MonadState s m => IndexedLensLike (Indexed i) ((,) r) s s a b -> (i -> a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,0)
l %%@= f = State.state (l %%@~ f)
#else
l %%@= f = do
  (r, s) <- State.gets (l %%@~ f)
  State.put s
  return r
#endif
{-# INLINE (%%@=) #-}

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the intermediate results.
--
-- @
-- ('<%@=') :: 'MonadState' s m                'IndexedLens' i s s a b      -> (i -> a -> b) -> m b
-- ('<%@=') :: ('MonadState' s m, 'Monoid' b) => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s s a b -> (i -> a -> b) -> m b
-- @
(<%@=) :: MonadState s m => IndexedLensLike (Indexed i) ((,) b) s s a b -> (i -> a -> b) -> m b
l <%@= f = l %%@= \ i a -> let b = f i a in (b, b)
{-# INLINE (<%@=) #-}

------------------------------------------------------------------------------
-- Reifying Indexed Lenses
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedLens i s t a b = ReifyIndexedLens { reflectIndexedLens :: IndexedLens i s t a b }

-- | @type 'ReifiedIndexedLens'' i = 'Simple' ('ReifiedIndexedLens' i)@
type ReifiedIndexedLens' i s a = ReifiedIndexedLens i s s a a

------------------------------------------------------------------------------
-- Deprecated
------------------------------------------------------------------------------

-- | @type 'SimpleIndexedLens' i = 'Simple' ('IndexedLens' i)@
type SimpleIndexedLens i s a = IndexedLens i s s a a
{-# DEPRECATED SimpleIndexedLens "use IndexedLens'" #-}

-- | @type 'SimpleReifiedIndexedLens' i = 'Simple' ('ReifiedIndexedLens' i)@
type SimpleReifiedIndexedLens i s a = ReifiedIndexedLens i s s a a
{-# DEPRECATED SimpleReifiedIndexedLens "use ReifiedIndexedLens'" #-}
