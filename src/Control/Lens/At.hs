{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.At
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.At
  (
  -- * Indexed Traversals
    Ixed(..)
  , _at -- DEPRECATED

  -- * Indexed Lenses
  , At(..)
  , Contains(..)
  ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.IndexedTraversal
import Control.Lens.Traversal
import Data.Array.IArray as Array
import Data.Array.Unboxed
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Set as Set
import Data.Sequence as Seq
import Data.Vector as Vector hiding (indexed)
import Data.Vector.Primitive as Prim
import Data.Vector.Storable as Storable

-- $setup
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- | A deprecated alias for 'el'
_at :: Ixed m => IxKey m -> SimpleIndexedTraversal (IxKey m) m (IxValue m)
_at = ix
{-# DEPRECATED _at "use 'ix'. '_at' will be removed in version 3.9" #-}

-- | This simple indexed traversal lets you 'traverse' the value at a given key in a map or element at an ordinal
-- position in a list or sequence.
class Ixed m where
  -- | What is the index type?
  type IxKey m :: *
  type IxValue m :: *
  -- | This simple indexed traversal lets you 'traverse' the value at a given key in a map.
  --
  -- *NB:* _setting_ the value of this 'Traversal' will only set the value in the lens
  -- if it is already present.
  --
  -- If you want to be able to insert /missing/ values, you want 'at'.
  --
  -- >>> Seq.fromList [a,b,c,d] & ix 2 %~ f
  -- fromList [a,b,f c,d]
  --
  -- >>> Seq.fromList [a,b,c,d] & ix 2 .~ e
  -- fromList [a,b,e,d]
  --
  -- >>> Seq.fromList [a,b,c,d] ^? ix 2
  -- Just c
  --
  -- >>> Seq.fromList [] ^? ix 2
  -- Nothing
  ix :: IxKey m -> SimpleIndexedTraversal (IxKey m) m (IxValue m)
#ifdef DEFAULT_SIGNATURES
  default ix :: At m => IxKey m -> SimpleIndexedTraversal (IxKey m) m (IxValue m)
  ix k = at k <. traverse
#endif

instance Ixed [a] where
  type IxKey [a] = Int
  type IxValue [a] = a
  ix k = indexed $ \ f xs0 ->
    let go [] _ = pure []
        go (a:as) 0 = f k a <&> (:as)
        go (a:as) i = (a:) <$> (go as $! i - 1)
    in go xs0 k
  {-# INLINE ix #-}

instance Ixed (Seq a) where
  type IxKey (Seq a) = Int
  type IxValue (Seq a) = a
  ix i = indexed $ \ f m ->
    if 0 <= i && i < Seq.length m
    then f i (Seq.index m i) <&> \a -> Seq.update i a m
    else pure m
  {-# INLINE ix #-}

instance Ixed (IntMap a) where
  type IxKey (IntMap a) = Int
  type IxValue (IntMap a) = a
  ix k = indexed $ \f m -> case IntMap.lookup k m of
     Just v -> f k v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance Ord k => Ixed (Map k a) where
  type IxKey (Map k a) = k
  type IxValue (Map k a) = a
  ix k = indexed $ \f m -> case Map.lookup k m of
     Just v  -> f k v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance (Eq k, Hashable k) => Ixed (HashMap k a) where
  type IxKey (HashMap k a) = k
  type IxValue (HashMap k a) = a
  ix k = indexed $ \f m -> case HashMap.lookup k m of
     Just v  -> f k v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

-- |
-- @
-- arr '!' i ≡ arr '^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i '.~' e '$' arr
-- @
instance Ix i => Ixed (Array i e) where
  type IxKey (Array i e) = i
  type IxValue (Array i e) = e
  ix i = indexed $ \f arr ->
    if inRange (bounds arr) i
    then f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else pure arr
  {-# INLINE ix #-}

-- |
-- @
-- arr '!' i ≡ arr '^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i '.~' e '$' arr
-- @
instance (IArray UArray e, Ix i) => Ixed (UArray i e) where
  type IxKey (UArray i e) = i
  type IxValue (UArray i e) = e
  ix i = indexed $ \f arr ->
    if inRange (bounds arr) i
    then f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else pure arr
  {-# INLINE ix #-}

instance Ixed (Vector.Vector a) where
  type IxKey (Vector.Vector a) = Int
  type IxValue (Vector.Vector a) = a
  ix i = indexed $ \f v ->
    if 0 <= i && i < Vector.length v
    then f i (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    else pure v
  {-# INLINE ix #-}

instance Prim a => Ixed (Prim.Vector a) where
  type IxKey (Prim.Vector a) = Int
  type IxValue (Prim.Vector a) = a
  ix i = indexed $ \f v ->
    if 0 <= i && i < Prim.length v
    then f i (v Prim.! i) <&> \a -> v Prim.// [(i, a)]
    else pure v
  {-# INLINE ix #-}

instance Storable a => Ixed (Storable.Vector a) where
  type IxKey (Storable.Vector a) = Int
  type IxValue (Storable.Vector a) = a
  ix i = indexed $ \f v ->
    if 0 <= i && i < Storable.length v
    then f i (v Storable.! i) <&> \a -> v Storable.// [(i, a)]
    else pure v
  {-# INLINE ix #-}

instance Ixed IntSet where
  type IxKey IntSet = Int
  type IxValue IntSet = Bool
  ix = contains
  {-# INLINE ix #-}

instance Ord a => Ixed (Set a) where
  type IxKey (Set a) = a
  type IxValue (Set a) = Bool
  ix = contains
  {-# INLINE ix #-}

instance (Eq a, Hashable a) => Ixed (HashSet a) where
  type IxKey (HashSet a) = a
  type IxValue (HashSet a) = Bool
  ix = contains
  {-# INLINE ix #-}

instance Eq k => Ixed (k -> a) where
  type IxKey (k -> a) = k
  type IxValue (k -> a) = a
  ix = contains
  {-# INLINE ix #-}

-- | 'At' provides a lens that can be used to read,
-- write or delete the value associated with a key in a map-like
-- container on an ad hoc basis.
--
-- An instance of @At@ should satisfy:
--
-- @'el' k ≡ 'at' k '<.' 'traverse'@
class Ixed m => At m where
  -- |
  -- >>> Map.fromList [(1,"world")] ^.at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at :: IxKey m -> SimpleIndexedLens (IxKey m) m (Maybe (IxValue m))

instance At (IntMap a) where
  at k = indexed $ \f m ->
    let mv = IntMap.lookup k m
    in f k mv <&> \r -> case r of
      Nothing -> maybe m (const (IntMap.delete k m)) mv
      Just v' -> IntMap.insert k v' m
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
  at k = indexed $ \f m ->
    let mv = Map.lookup k m
    in f k mv <&> \r -> case r of
      Nothing -> maybe m (const (Map.delete k m)) mv
      Just v' -> Map.insert k v' m
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At (HashMap k a) where
  at k = indexed $ \f m ->
    let mv = HashMap.lookup k m
    in f k mv <&> \r -> case r of
      Nothing -> maybe m (const (HashMap.delete k m)) mv
      Just v' -> HashMap.insert k v' m
  {-# INLINE at #-}

-- | Provides an 'IndexedLens' that can be used to read, write to a 'total map'.
class Ixed m => Contains m where
  -- |
  -- If this wasn't an indexed lens you could assume:
  --
  -- @'contains' k ≡ 'singular' ('ix' k)@
  --
  -- >>> contains 3 .~ False $ IntSet.fromList [1,2,3,4]
  -- fromList [1,2,4]
  contains :: IxKey m -> SimpleIndexedLens (IxKey m) m (IxValue m)

instance Contains IntSet where
  contains k = indexed $ \ f s -> f k (IntSet.member k s) <&> \b ->
    if b then IntSet.insert k s else IntSet.delete k s
  {-# INLINE contains #-}

instance Ord k => Contains (Set k) where
  contains k = indexed $ \ f s -> f k (Set.member k s) <&> \b ->
    if b then Set.insert k s else Set.delete k s
  {-# INLINE contains #-}

instance (Eq k, Hashable k) => Contains (HashSet k) where
  contains k = indexed $ \ f s -> f k (HashSet.member k s) <&> \b ->
    if b then HashSet.insert k s else HashSet.delete k s
  {-# INLINE contains #-}

instance Eq k => Contains (k -> a) where
  contains e = indexed $ \ g f -> g e (f e) <&> \a' e' -> if e == e' then a' else f e'
  {-# INLINE contains #-}
