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
import Data.Array as Array
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Sequence as Seq
import Data.Traversable
import Data.Vector as Vector hiding (indexed)

-- $setup
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g


-- | A deprecated alias for 'el'
_at :: El k m => k -> SimpleIndexedTraversal k (m v) v
_at = el
{-# DEPRECATED _at "use 'el'. '_at' will be removed in version 3.9" #-}

-- | This simple indexed traversal lets you 'traverse' the value at a given key in a map or element at an ordinal
-- position in a list or sequence.
class El k m | m -> k where
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
  el :: k -> SimpleIndexedTraversal k (m v) v
#ifdef DEFAULT_SIGNATURES
  default el :: At k m => k -> SimpleIndexedTraversal k (m v) v
  el k = at k <. traverse
#endif


instance El Int [] where
  el k = indexed $ \ f xs0 ->
    let go [] _ = pure []
        go (a:as) 0 = f k a <&> (:as)
        go (a:as) i = (a:) <$> (go as $! i - 1)
    in go xs0 k
  {-# INLINE el #-}

instance El Int Seq where
  el i = indexed $ \ f m ->
    if 0 <= i && i < Seq.length m
    then f i (Seq.index m i) <&> \a -> Seq.update i a m
    else pure m
  {-# INLINE el #-}

instance El Int IntMap where
  el k = indexed $ \f m -> case IntMap.lookup k m of
     Just v -> f k v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE el #-}

instance Ord k => El k (Map k) where
  el k = indexed $ \f m -> case Map.lookup k m of
     Just v  -> f k v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE el #-}

instance (Eq k, Hashable k) => El k (HashMap k) where
  el k = indexed $ \f m -> case HashMap.lookup k m of
     Just v  -> f k v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE el #-}

-- |
-- @
-- arr '!' i ≡ arr '^.' 'el' i
-- arr '//' [(i,e)] ≡ 'el' i '.~' e '$' arr
-- @
instance Ix i => El i (Array i) where
  el i = indexed $ \f arr ->
    if inRange (bounds arr) i
    then f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else pure arr
  {-# INLINE el #-}

instance El Int Vector where
  el i = indexed $ \f v ->
    if 0 <= i && i < Vector.length v
    then f i (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    else pure v

-- | 'At' provides a lens that can be used to read,
-- write or delete the value associated with a key in a map-like
-- container on an ad hoc basis.
--
-- An instance of @At@ should satisfy:
--
-- @'el' k ≡ 'at' k '<.' 'traverse'@
class El k m => At k m | m -> k where
  -- |
  -- >>> IntMap.fromList [(1,"hello")] ^.at 1
  -- Just "hello"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- Note: 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at :: k -> SimpleIndexedLens k (m v) (Maybe v)

instance At Int IntMap where
  at k = indexed $ \f m ->
    let mv = IntMap.lookup k m
        go Nothing   = maybe m (const (IntMap.delete k m)) mv
        go (Just v') = IntMap.insert k v' m
    in go <$> f k mv where
  {-# INLINE at #-}

instance Ord k => At k (Map k) where
  at k = indexed $ \f m ->
    let mv = Map.lookup k m
        go Nothing   = maybe m (const (Map.delete k m)) mv
        go (Just v') = Map.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At k (HashMap k) where
  at k = indexed $ \f m ->
    let mv = HashMap.lookup k m
        go Nothing   = maybe m (const (HashMap.delete k m)) mv
        go (Just v') = HashMap.insert k v' m
    in go <$> f k mv
  {-# INLINE at #-}
