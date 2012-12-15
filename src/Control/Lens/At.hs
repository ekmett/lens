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
    El(..)
  , At(..)
  , _at -- DEPRECATED
  ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.IndexedTraversal
import Data.Array.IArray as Array
import Data.Array.Unboxed
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Sequence as Seq
import Data.Traversable
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
_at :: El m => ElKey m -> SimpleIndexedTraversal (ElKey m) m (ElValue m)
_at = el
{-# DEPRECATED _at "use 'el'. '_at' will be removed in version 3.9" #-}

-- | This simple indexed traversal lets you 'traverse' the value at a given key in a map or element at an ordinal
-- position in a list or sequence.
class El m where
  -- | What is the index type?
  type ElKey m :: *
  type ElValue m :: *
  -- | This simple indexed traversal lets you 'traverse' the value at a given key in a map.
  --
  -- *NB:* _setting_ the value of this 'Traversal' will only set the value in the lens
  -- if it is already present.
  --
  -- If you want to be able to insert /missing/ values, you want 'at'.
  --
  -- >>> Seq.fromList [a,b,c,d] & el 2 %~ f
  -- fromList [a,b,f c,d]
  --
  -- >>> Seq.fromList [a,b,c,d] & el 2 .~ e
  -- fromList [a,b,e,d]
  --
  -- >>> Seq.fromList [a,b,c,d] ^? el 2
  -- Just c
  --
  -- >>> Seq.fromList [] ^? el 2
  -- Nothing
  el :: ElKey m -> SimpleIndexedTraversal (ElKey m) m (ElValue m)
#ifdef DEFAULT_SIGNATURES
  default el :: At m => ElKey m -> SimpleIndexedTraversal (ElKey m) m (ElValue m)
  el k = at k <. traverse
#endif

instance El [a] where
  type ElKey [a] = Int
  type ElValue [a] = a
  el k = indexed $ \ f xs0 ->
    let go [] _ = pure []
        go (a:as) 0 = f k a <&> (:as)
        go (a:as) i = (a:) <$> (go as $! i - 1)
    in go xs0 k
  {-# INLINE el #-}

instance El (Seq a) where
  type ElKey (Seq a) = Int
  type ElValue (Seq a) = a
  el i = indexed $ \ f m ->
    if 0 <= i && i < Seq.length m
    then f i (Seq.index m i) <&> \a -> Seq.update i a m
    else pure m
  {-# INLINE el #-}

instance El (IntMap a) where
  type ElKey (IntMap a) = Int
  type ElValue (IntMap a) = a
  el k = indexed $ \f m -> case IntMap.lookup k m of
     Just v -> f k v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE el #-}

instance Ord k => El (Map k a) where
  type ElKey (Map k a) = k
  type ElValue (Map k a) = a
  el k = indexed $ \f m -> case Map.lookup k m of
     Just v  -> f k v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE el #-}

instance (Eq k, Hashable k) => El (HashMap k a) where
  type ElKey (HashMap k a) = k
  type ElValue (HashMap k a) = a
  el k = indexed $ \f m -> case HashMap.lookup k m of
     Just v  -> f k v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE el #-}

-- |
-- @
-- arr '!' i ≡ arr '^.' 'el' i
-- arr '//' [(i,e)] ≡ 'el' i '.~' e '$' arr
-- @
instance Ix i => El (Array i e) where
  type ElKey (Array i e) = i
  type ElValue (Array i e) = e
  el i = indexed $ \f arr ->
    if inRange (bounds arr) i
    then f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else pure arr
  {-# INLINE el #-}

-- |
-- @
-- arr '!' i ≡ arr '^.' 'el' i
-- arr '//' [(i,e)] ≡ 'el' i '.~' e '$' arr
-- @
instance (IArray UArray e, Ix i) => El (UArray i e) where
  type ElKey (UArray i e) = i
  type ElValue (UArray i e) = e
  el i = indexed $ \f arr ->
    if inRange (bounds arr) i
    then f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else pure arr
  {-# INLINE el #-}

instance El (Vector.Vector a) where
  type ElKey (Vector.Vector a) = Int
  type ElValue (Vector.Vector a) = a
  el i = indexed $ \f v ->
    if 0 <= i && i < Vector.length v
    then f i (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    else pure v
  {-# INLINE el #-}

instance Prim a => El (Prim.Vector a) where
  type ElKey (Prim.Vector a) = Int
  type ElValue (Prim.Vector a) = a
  el i = indexed $ \f v ->
    if 0 <= i && i < Prim.length v
    then f i (v Prim.! i) <&> \a -> v Prim.// [(i, a)]
    else pure v
  {-# INLINE el #-}

instance Storable a => El (Storable.Vector a) where
  type ElKey (Storable.Vector a) = Int
  type ElValue (Storable.Vector a) = a
  el i = indexed $ \f v ->
    if 0 <= i && i < Storable.length v
    then f i (v Storable.! i) <&> \a -> v Storable.// [(i, a)]
    else pure v
  {-# INLINE el #-}

-- | 'At' provides a lens that can be used to read,
-- write or delete the value associated with a key in a map-like
-- container on an ad hoc basis.
--
-- An instance of @At@ should satisfy:
--
-- @'el' k ≡ 'at' k '<.' 'traverse'@
class El m => At m where
  -- |
  -- >>> Map.fromList [(1,"world")] ^.at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at :: ElKey m -> SimpleIndexedLens (ElKey m) m (Maybe (ElValue m))

instance At (IntMap a) where
  at k = indexed $ \f m ->
    let mv = IntMap.lookup k m
        go Nothing   = maybe m (const (IntMap.delete k m)) mv
        go (Just v') = IntMap.insert k v' m
    in go <$> f k mv where
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
  at k = indexed $ \f m ->
    let mv = Map.lookup k m
        go Nothing   = maybe m (const (Map.delete k m)) mv
        go (Just v') = Map.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At (HashMap k a) where
  at k = indexed $ \f m ->
    let mv = HashMap.lookup k m
        go Nothing   = maybe m (const (HashMap.delete k m)) mv
        go (Just v') = HashMap.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}
