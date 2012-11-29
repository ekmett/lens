{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Projection
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.IndexedProjection
  (
  -- * Projections
    IndexedProjection
  , IndexedProjectiveLens
  -- * Constructing Projections
  , IndexedProjective(..)
  , IndexedProjecting

  -- * Common indexed projective lenses
  , At(..)
  , Contains(..)
  , resultAt

{-
  -- * Consuming Projections
  , cloneIndexedProjection
-}

  -- * Simple
  , SimpleIndexedProjection
  , SimpleIndexedProjectiveLens
  ) where

import Control.Applicative
import Control.Lens.Classes
import Control.Lens.Combinators
import Control.Lens.Internal
import Control.Lens.Type
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Set as Set

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Indexed Projection Internals
------------------------------------------------------------------------------

-- | An 'IndexedProjection' @l@ is a 0-or-1 target 'IndexedTraversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction, such that in addition to the 'IndexedTraversal' laws, we also
-- have the 'Projection' laws.
--
-- Every 'IndexedProjection' is a valid 'IndexedTraversal'.
--
-- You should be able to pass an 'IndexedProjection' to any combinator that actually consumes a 'Projection' as a
-- 'Projecting'. If you need to pass it as a 'Projection', you may have to call 'cloneProjection' on it first.
type IndexedProjection i s t a b = forall k f. (IndexedProjective i k, Applicative f) => k (a -> f b) (s -> f t)

-- | An 'IndexedProjectiveLens' @l@ is an 'IndexedLens' than can also be used as an 'IndexedProjection'
type IndexedProjectiveLens i s t a b = forall k f. (IndexedProjective i k, Functor f) => k (a -> f b) (s -> f t)

-- | A @'Simple' ('IndexedProjection' i)@
type SimpleIndexedProjection i s a = IndexedProjection i s s a a

-- | A @'Simple' ('IndexedProjectiveLens' i)@
type SimpleIndexedProjectiveLens i s a = IndexedProjectiveLens i s s a a

-- | Consume a 'Project'. This is commonly used when a function takes a 'Projection' as a parameter.
type IndexedProjecting i f s t a b = Overloaded (IndexedProject i) f s t a b

-- | Provides a 'SimpleIndexedProjectiveLens' that can be used to read, write or delete the value associated with a key in a map-like container or construct an empty or singleton map.
class At k m | m -> k where
  -- |
  -- >>> Map.fromList [(1,"hello")] ^.at 1
  -- Just "hello"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  at :: k -> SimpleIndexedProjectiveLens k (m v) (Maybe v)

  -- | This simple indexed projection lets you 'project' the value at a given key in a map.
  --
  -- For most uses, you can view this as the simpler 'Traversal':
  --
  -- @'_at' k ≡ 'at' k '.' 'traverse'@
  _at :: At k m => k -> SimpleIndexedProjection k (m v) v

instance At Int IntMap where
  at k = iprojecting (maybe IntMap.empty (IntMap.singleton k)) $ \f m ->
    let mv = IntMap.lookup k m
        go Nothing   = maybe m (const (IntMap.delete k m)) mv
        go (Just v') = IntMap.insert k v' m
    in go <$> f k mv where
  {-# INLINE at #-}
  _at k = iprojecting (IntMap.singleton k) $ \f m -> case IntMap.lookup k m of
     Just v -> f k v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE _at #-}

instance Ord k => At k (Map k) where
  at k = iprojecting (maybe Map.empty (Map.singleton k)) $ \f m ->
    let mv = Map.lookup k m
        go Nothing   = maybe m (const (Map.delete k m)) mv
        go (Just v') = Map.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}
  _at k = iprojecting (Map.singleton k) $ \f m -> case Map.lookup k m of
     Just v  -> f k v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE _at #-}

instance (Eq k, Hashable k) => At k (HashMap k) where
  at k = iprojecting (maybe HashMap.empty (HashMap.singleton k)) $ \f m ->
    let mv = HashMap.lookup k m
        go Nothing   = maybe m (const (HashMap.delete k m)) mv
        go (Just v') = HashMap.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}
  _at k = iprojecting (HashMap.singleton k) $ \f m -> case HashMap.lookup k m of
     Just v  -> f k v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE _at #-}

-- | Provides an 'IndexedLens' that can be used to read, write or delete a member of a set-like container
class Contains k m | m -> k where
  -- |
  -- >>> contains 3 .~ False $ IntSet.fromList [1,2,3,4]
  -- fromList [1,2,4]
  contains :: k -> SimpleIndexedProjectiveLens k m Bool

instance Contains Int IntSet where
  contains k = iprojecting embedding $ \ f s -> f k (IntSet.member k s) <&> \b -> if b then IntSet.insert k s else IntSet.delete k s 
    where
      embedding False = IntSet.empty
      embedding True = IntSet.singleton k
  {-# INLINE contains #-}

instance Ord k => Contains k (Set k) where
  contains k = iprojecting embedding $ \ f s -> f k (Set.member k s) <&> \b -> if b then Set.insert k s else Set.delete k s
    where
      embedding False = Set.empty
      embedding True = Set.singleton k
  {-# INLINE contains #-}

instance (Eq k, Hashable k) => Contains k (HashSet k) where
  contains k = iprojecting embedding $ \ f s -> f k (HashSet.member k s) <&> \b -> if b then HashSet.insert k s else HashSet.delete k s
    where
      embedding False = HashSet.empty
      embedding True = HashSet.singleton k
  {-# INLINE contains #-}

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
--
-- >>> let f = (+1) & resultAt 3 .~ 8 in (f 2, f 3)
-- (3,8)
resultAt :: Eq e => e -> SimpleIndexedProjectiveLens e (e -> a) a
resultAt e = iprojecting const $ \ g f -> g e (f e) <&> \a' e' -> if e == e' then a' else f e'
{-# INLINE resultAt #-}

{-
-- | Clone a 'Projection' so that you can reuse the same monomorphically typed 'Projection' for different purposes.
--
-- See 'cloneLens' and 'cloneTraversal' for examples of why you might want to do this.
cloneIndexedProjection :: Overloaded (IndexedProject i) (Bazaar a b) s t a b -> IndexedProjection i s t a b
cloneIndexedProjection (IndexedProject f g) = iprojecting (unsafeCoerce f) (cloneIndexedTraversal (unsafeCoerce g))

-- | Compose an 'Indexed' function with a non-indexed function.
--
-- Mnemonically, the @<@ points to the indexing we want to preserve.
(<@.)  :: forall i k f a b x y s t. IndexedProjective i k => IndexedProject i (x -> f y) (s -> f t) -> ((a -> f b) -> (x -> f y)) -> k (a -> f b) (s -> f t)
-}
