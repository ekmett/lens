{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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
  , (%%@~)
  , (<%@~)
  , (%%@=)
  , (<%@=)

  -- * Simple
  , SimpleIndexedLens
  ) where

import Control.Lens.Indexed
import Control.Lens.Type
import Control.Monad.State.Class as State

infixr 4 %%@~, <%@~
infix  4 %%@=, <%@=

-- | Every 'IndexedLens' is a valid 'Lens' and a valid 'Control.Lens.IndexedTraversal.IndexedTraversal'.
type IndexedLens i a b c d = forall f k. (Indexed i k, Functor f) => k (c -> f d) (a -> f b)

-- | @type 'SimpleIndexedLens' i = 'Simple' ('IndexedLens' i)@
type SimpleIndexedLens i a b = IndexedLens i a a b b

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' and return a monoidal summary
-- along with the answer.
--
-- @l '<%~' f = l '<%@~' 'const' f@
--
-- When you do not need access to the index then ('<%~') is more liberal in what it can accept.
--
-- If you do not need the intermediate result, you can use ('Control.Lens.Type.%@~') or even ('Control.Lens.Type.%~').
--
-- @
-- ('\<%\@~') ::             'IndexedLens' i a b c d -> (i -> c -> d) -> a -> (d, b)
-- ('\<%\@~') :: 'Monoid' d => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> d) -> a -> (d, b)
-- @
(<%@~) :: Overloaded (Index i) ((,)d) a b c d -> (i -> c -> d) -> a -> (d, b)
l <%@~ f = withIndex l $ \i c -> let d = f i c in (d, d)
{-# INLINE (<%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' and return a monoidal summary
-- of the supplementary results and the answer.
--
-- @('%%@~') = 'withIndex'@
--
-- @
-- ('%%\@~') :: 'Functor' f => 'IndexedLens' i a b c d      -> (i -> c -> f d) -> a -> f b
-- ('%%\@~') :: 'Functor' f => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> f d) -> a -> f b
-- @
--
-- In particular, it is often useful to think of this function as having one of these even more
-- restrictive type signatures
--
-- @
-- ('%%\@~') ::             'IndexedLens' i a b c d      -> (i -> c -> (e, d)) -> a -> (e, b)
-- ('%%\@~') :: 'Monoid' e => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a b c d -> (i -> c -> (e, d)) -> a -> (e, b)
-- @
(%%@~) :: Overloaded (Index i) f a b c d -> (i -> c -> f d) -> a -> f b
(%%@~) = withIndex
{-# INLINE (%%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the supplementary results.
--
-- @l '%%@=' f = 'state' (l '%%@~' f)@
--
-- @
-- ('%%\@=') :: 'MonadState' a m                'IndexedLens' i a a c d      -> (i -> c -> (e, d)) -> a -> m e
-- ('%%\@=') :: ('MonadState' a m, 'Monoid' e) => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a a c d -> (i -> c -> (e, d)) -> a -> m e
-- @
(%%@=) :: MonadState a m => Overloaded (Index i) ((,)e) a a c d -> (i -> c -> (e, d)) -> m e
#if MIN_VERSION_mtl(2,1,0)
l %%@= f = State.state (l %%@~ f)
#else
l %%@= f = do
  (e, d) <- State.gets (l %%@~ f)
  State.put d
  return e
#endif
{-# INLINE (%%@=) #-}

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the intermediate results.
--
-- @
-- ('\<%\@=') :: 'MonadState' a m                'IndexedLens' i a a c d      -> (i -> c -> d) -> a -> m d
-- ('\<%\@=') :: ('MonadState' a m, 'Monoid' e) => 'Control.Lens.IndexedTraversal.IndexedTraversal' i a a c d -> (i -> c -> d) -> a -> m d
-- @
(<%@=) :: MonadState a m => Overloaded (Index i) ((,)d) a a c d -> (i -> c -> d) -> m d
l <%@= f = l %%@= \ i c -> let d = f i c in (d, d)
{-# INLINE (<%@=) #-}
