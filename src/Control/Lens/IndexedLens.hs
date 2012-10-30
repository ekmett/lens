{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
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
  -- * Common Indexed Lenses
  , At(..)
  , Contains(..)
  -- * Indexed Lens Combinators
  , (%%@~)
  , (<%@~)
  , (%%@=)
  , (<%@=)
  -- * Storing Indexed Lenses
  , ReifiedIndexedLens(..)
  -- * Simple
  , SimpleIndexedLens
  , SimpleReifiedIndexedLens
  ) where

import Control.Applicative
import Control.Lens.Indexed
import Control.Lens.Type
import Control.Monad.State.Class as State
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.IntMap as IntMap
import Data.Map as Map

import Data.HashSet as HashSet
import Data.IntSet as IntSet
import Data.Set as Set

-- $setup
-- >>> import Control.Lens

infixr 4 %%@~, <%@~
infix  4 %%@=, <%@=

-- | Every 'IndexedLens' is a valid 'Lens' and a valid 'Control.Lens.IndexedTraversal.IndexedTraversal'.
type IndexedLens i s t a b = forall f k. (Indexed i k, Functor f) => k (a -> f b) (s -> f t)

-- | @type 'SimpleIndexedLens' i = 'Simple' ('IndexedLens' i)@
type SimpleIndexedLens i s a = IndexedLens i s s a a

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
-- ('<%@~') ::             'IndexedLens' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- ('<%@~') :: 'Monoid' b => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- @
(<%@~) :: Overloaded (Index i) ((,)b) s t a b -> (i -> a -> b) -> s -> (b, t)
l <%@~ f = withIndex l $ \i a -> let b = f i a in (b, b)
{-# INLINE (<%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' and return a monoidal summary
-- of the supplementary results and the answer.
--
-- @('%%@~') = 'withIndex'@
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
(%%@~) :: Overloaded (Index i) f s t a b -> (i -> a -> f b) -> s -> f t
(%%@~) = withIndex
{-# INLINE (%%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.IndexedTraversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the supplementary results.
--
-- @l '%%@=' f = 'state' (l '%%@~' f)@
--
-- @
-- ('%%@=') :: 'MonadState' s m                'IndexedLens' i s s a b      -> (i -> a -> (r, b)) -> s -> m r
-- ('%%@=') :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.IndexedTraversal.IndexedTraversal' i s s a b -> (i -> a -> (r, b)) -> s -> m r
-- @
(%%@=) :: MonadState s m => Overloaded (Index i) ((,)r) s s a b -> (i -> a -> (r, b)) -> m r
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
(<%@=) :: MonadState s m => Overloaded (Index i) ((,)b) s s a b -> (i -> a -> b) -> m b
l <%@= f = l %%@= \ i a -> let b = f i a in (b, b)
{-# INLINE (<%@=) #-}

-- | Provides an 'IndexedLens' that can be used to read, write or delete the value associated with a key in a map-like container.
class At k m | m -> k where
  -- |
  -- >>> Map.fromList [(1,"hello")] ^.at 1
  -- Just "hello"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  at :: k -> SimpleIndexedLens k (m v) (Maybe v)

instance At Int IntMap where
  at k = index $ \ f m -> (`go` m) <$> f k (IntMap.lookup k m) where
    go Nothing   = IntMap.delete k
    go (Just v') = IntMap.insert k v'
  {-# INLINE at #-}

instance Ord k => At k (Map k) where
  at k = index $ \ f m -> (`go` m) <$> f k (Map.lookup k m) where
    go Nothing   = Map.delete k
    go (Just v') = Map.insert k v'
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At k (HashMap k) where
  at k = index $ \ f m -> (`go` m) <$> f k (HashMap.lookup k m) where
    go Nothing   = HashMap.delete k
    go (Just v') = HashMap.insert k v'
  {-# INLINE at #-}

-- | Provides an 'IndexedLens' that can be used to read, write or delete a member of a set-like container
class Contains k m | m -> k where
  -- |
  -- >>> contains 3 .~ False $ IntSet.fromList [1,2,3,4]
  -- fromList [1,2,4]
  contains :: k -> SimpleIndexedLens k m Bool

instance Contains Int IntSet where
  contains k = index $ \ f s -> (\b -> if b then IntSet.insert k s else IntSet.delete k s) <$> f k (IntSet.member k s)
  {-# INLINE contains #-}

instance Ord k => Contains k (Set k) where
  contains k = index $ \ f s -> (\b -> if b then Set.insert k s else Set.delete k s) <$> f k (Set.member k s)
  {-# INLINE contains #-}

instance (Eq k, Hashable k) => Contains k (HashSet k) where
  contains k = index $ \ f s -> (\b -> if b then HashSet.insert k s else HashSet.delete k s) <$> f k (HashSet.member k s)
  {-# INLINE contains #-}

------------------------------------------------------------------------------
-- Reifying Indexed Lenses
------------------------------------------------------------------------------

-- | Useful for storage.
newtype ReifiedIndexedLens i s t a b = ReifyIndexedLens { reflectIndexedLens :: IndexedLens i s t a b }

-- | @type 'SimpleIndexedLens' i = 'Simple' ('ReifiedIndexedLens' i)@
type SimpleReifiedIndexedLens i s a = ReifiedIndexedLens i s s a a
