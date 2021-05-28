{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.At
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.At
  (
  -- * At
    At(at)
    , sans
    , iat
  -- * Ixed
  , Index
  , IxValue
  , Ixed(ix)
  , ixAt
  , iix
  -- * Contains
  , Contains(contains)
  , icontains
  ) where

import Prelude ()

import Control.Lens.Each
import Control.Lens.Internal.Prelude
import Control.Lens.Traversal
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.Indexed
import Control.Monad (guard)
import Data.Array.IArray as Array
import Data.Array.Unboxed
import qualified Data.ByteString as StrictB
import qualified Data.ByteString.Lazy as LazyB
import Data.Complex
import Data.Functor (($>))
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Int
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.Kind
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text as StrictT
import qualified Data.Text.Lazy as LazyT
import Data.Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Primitive as Prim
import Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import Data.Vector.Unboxed (Unbox)
import Data.Word
import Foreign.Storable (Storable)

type family Index (s :: Type) :: Type
type instance Index (e -> a) = e
type instance Index IntSet = Int
type instance Index (Set a) = a
type instance Index (HashSet a) = a
type instance Index [a] = Int
type instance Index (NonEmpty a) = Int
type instance Index (Seq a) = Int
type instance Index (a,b) = Int
type instance Index (a,b,c) = Int
type instance Index (a,b,c,d) = Int
type instance Index (a,b,c,d,e) = Int
type instance Index (a,b,c,d,e,f) = Int
type instance Index (a,b,c,d,e,f,g) = Int
type instance Index (a,b,c,d,e,f,g,h) = Int
type instance Index (a,b,c,d,e,f,g,h,i) = Int
type instance Index (IntMap a) = Int
type instance Index (Map k a) = k
type instance Index (HashMap k a) = k
type instance Index (Array.Array i e) = i
type instance Index (UArray i e) = i
type instance Index (Vector.Vector a) = Int
type instance Index (Prim.Vector a) = Int
type instance Index (Storable.Vector a) = Int
type instance Index (Unboxed.Vector a) = Int
type instance Index (Complex a) = Int
type instance Index (Identity a) = ()
type instance Index (Maybe a) = ()
type instance Index (Tree a) = [Int]
type instance Index StrictT.Text = Int
type instance Index LazyT.Text = Int64
type instance Index StrictB.ByteString = Int
type instance Index LazyB.ByteString = Int64

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import qualified Data.IntSet as IntSet
-- >>> import qualified Data.Sequence as Seq
-- >>> import qualified Data.Map as Map
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f  :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g  :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let f' :: Int -> Expr -> Expr; f' = Debug.SimpleReflect.Vars.f'
-- >>> let h  :: Int -> Expr; h = Debug.SimpleReflect.Vars.h

-- |
-- This class provides a simple 'Lens' that lets you view (and modify)
-- information about whether or not a container contains a given 'Index'.
class Contains m where
  -- |
  -- >>> IntSet.fromList [1,2,3,4] ^. contains 3
  -- True
  --
  -- >>> IntSet.fromList [1,2,3,4] ^. contains 5
  -- False
  --
  -- >>> IntSet.fromList [1,2,3,4] & contains 3 .~ False
  -- fromList [1,2,4]
  contains :: Index m -> Lens' m Bool

-- | An indexed version of 'contains'.
--
-- >>> IntSet.fromList [1,2,3,4] ^@. icontains 3
-- (3,True)
--
-- >>> IntSet.fromList [1,2,3,4] ^@. icontains 5
-- (5,False)
--
-- >>> IntSet.fromList [1,2,3,4] & icontains 3 %@~ \i x -> if odd i then not x else x
-- fromList [1,2,4]
--
-- >>> IntSet.fromList [1,2,3,4] & icontains 3 %@~ \i x -> if even i then not x else x
-- fromList [1,2,3,4]
icontains :: Contains m => Index m -> IndexedLens' (Index m) m Bool
icontains i f = contains i (indexed f i)
{-# INLINE icontains #-}

instance Contains IntSet where
#if MIN_VERSION_containers(0,6,3)
  contains k f = IntSet.alterF f k
#else
  -- This is a flipped copy of the implementation of `IntSet.alterF`.  Unlike a
  -- `Set`, we don't have to worry about expensive comparisons from descending
  -- multiple times into an `IntSet`. We are careful to share the results of
  -- insertion or deletion across multiple positions in the `Functor`.
  contains k f s = fmap choose (f member_)
    where
      member_ = IntSet.member k s

      (inserted, deleted)
        | member_   = (s, IntSet.delete k s)
        | otherwise = (IntSet.insert k s, s)

      choose True  = inserted
      choose False = deleted
#endif
  {-# INLINE contains #-}

instance Ord a => Contains (Set a) where
#if MIN_VERSION_containers(0,6,3)
  contains k f = Set.alterF f k
#else
  contains k f s = f (Set.member k s) <&> \b ->
    if b then Set.insert k s else Set.delete k s
#endif
  {-# INLINE contains #-}

instance (Eq a, Hashable a) => Contains (HashSet a) where
  contains k f s = HashSet.fromMap <$>
    HashMap.alterF (fmap guard . f . isJust) k (HashSet.toMap s)
  {-# INLINE contains #-}

-- | This provides a common notion of a value at an index that is shared by both 'Ixed' and 'At'.
type family IxValue (m :: Type) :: Type

-- | Provides a simple 'Traversal' lets you 'traverse' the value at a given
-- key in a 'Map' or element at an ordinal position in a list or 'Seq'.
class Ixed m where
  -- |
  -- /NB:/ Setting the value of this 'Traversal' will only set the value in
  -- 'at' if it is already present.
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
  ix :: Index m -> Traversal' m (IxValue m)
  default ix :: At m => Index m -> Traversal' m (IxValue m)
  ix = ixAt
  {-# INLINE ix #-}

-- | An indexed version of 'ix'.
--
-- >>> Seq.fromList [a,b,c,d] & iix 2 %@~ f'
-- fromList [a,b,f' 2 c,d]
--
-- >>> Seq.fromList [a,b,c,d] & iix 2 .@~ h
-- fromList [a,b,h 2,d]
--
-- >>> Seq.fromList [a,b,c,d] ^@? iix 2
-- Just (2,c)
--
-- >>> Seq.fromList [] ^@? iix 2
-- Nothing
iix :: Ixed m => Index m -> IndexedTraversal' (Index m) m (IxValue m)
iix i f = ix i (indexed f i)
{-# INLINE iix #-}

-- | A definition of 'ix' for types with an 'At' instance. This is the default
-- if you don't specify a definition for 'ix'.
ixAt :: At m => Index m -> Traversal' m (IxValue m)
ixAt i = at i . traverse
{-# INLINE ixAt #-}

type instance IxValue (e -> a) = a
instance Eq e => Ixed (e -> a) where
  ix e p f = p (f e) <&> \a e' -> if e == e' then a else f e'
  {-# INLINE ix #-}

type instance IxValue (Maybe a) = a
instance Ixed (Maybe a) where
  ix ~() f (Just a) = Just <$> f a
  ix ~() _ Nothing  = pure Nothing
  {-# INLINE ix #-}

type instance IxValue [a] = a
instance Ixed [a] where
  ix k f xs0 | k < 0     = pure xs0
             | otherwise = go xs0 k where
    go [] _ = pure []
    go (a:as) 0 = f a <&> (:as)
    go (a:as) i = (a:) <$> (go as $! i - 1)
  {-# INLINE ix #-}

type instance IxValue (NonEmpty a) = a
instance Ixed (NonEmpty a) where
  ix k f xs0 | k < 0 = pure xs0
             | otherwise = go xs0 k where
    go (a:|as) 0 = f a <&> (:|as)
    go (a:|as) i = (a:|) <$> ix (i - 1) f as
  {-# INLINE ix #-}

type instance IxValue (Identity a) = a
instance Ixed (Identity a) where
  ix ~() f (Identity a) = Identity <$> f a
  {-# INLINE ix #-}

type instance IxValue (Tree a) = a
instance Ixed (Tree a) where
  ix xs0 f = go xs0 where
    go [] (Node a as) = f a <&> \a' -> Node a' as
    go (i:is) t@(Node a as)
      | i < 0     = pure t
      | otherwise = Node a <$> ix i (go is) as
  {-# INLINE ix #-}

type instance IxValue (Seq a) = a
instance Ixed (Seq a) where
  ix i f m
    | 0 <= i && i < Seq.length m = f (Seq.index m i) <&> \a -> Seq.update i a m
    | otherwise                  = pure m
  {-# INLINE ix #-}

type instance IxValue (IntMap a) = a
instance Ixed (IntMap a) where
  ix k f m = case IntMap.lookup k m of
     Just v -> f v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

type instance IxValue (Map k a) = a
instance Ord k => Ixed (Map k a) where
  ix k f m = case Map.lookup k m of
     Just v  -> f v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

type instance IxValue (HashMap k a) = a
instance (Eq k, Hashable k) => Ixed (HashMap k a) where
  ix k f m = case HashMap.lookup k m of
     Just v  -> f v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

type instance IxValue (Set k) = ()
instance Ord k => Ixed (Set k) where
  ix k f m = if Set.member k m
     then f () $> m
     else pure m
  {-# INLINE ix #-}

type instance IxValue IntSet = ()
instance Ixed IntSet where
  ix k f m = if IntSet.member k m
     then f () $> m
     else pure m
  {-# INLINE ix #-}

type instance IxValue (HashSet k) = ()
instance (Eq k, Hashable k) => Ixed (HashSet k) where
  ix k f m = if HashSet.member k m
     then f () $> m
     else pure m
  {-# INLINE ix #-}

type instance IxValue (Array.Array i e) = e
-- |
-- @
-- arr '!' i ≡ arr 'Control.Lens.Getter.^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i 'Control.Lens.Setter..~' e '$' arr
-- @
instance Ix i => Ixed (Array.Array i e) where
  ix i f arr
    | inRange (bounds arr) i = f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

type instance IxValue (UArray i e) = e
-- |
-- @
-- arr '!' i ≡ arr 'Control.Lens.Getter.^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i 'Control.Lens.Setter..~' e '$' arr
-- @
instance (IArray UArray e, Ix i) => Ixed (UArray i e) where
  ix i f arr
    | inRange (bounds arr) i = f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

type instance IxValue (Vector.Vector a) = a
instance Ixed (Vector.Vector a) where
  ix i f v
    | 0 <= i && i < Vector.length v = f (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    | otherwise                     = pure v
  {-# INLINE ix #-}

type instance IxValue (Prim.Vector a) = a
instance Prim a => Ixed (Prim.Vector a) where
  ix i f v
    | 0 <= i && i < Prim.length v = f (v Prim.! i) <&> \a -> v Prim.// [(i, a)]
    | otherwise                   = pure v
  {-# INLINE ix #-}

type instance IxValue (Storable.Vector a) = a
instance Storable a => Ixed (Storable.Vector a) where
  ix i f v
    | 0 <= i && i < Storable.length v = f (v Storable.! i) <&> \a -> v Storable.// [(i, a)]
    | otherwise                       = pure v
  {-# INLINE ix #-}

type instance IxValue (Unboxed.Vector a) = a
instance Unbox a => Ixed (Unboxed.Vector a) where
  ix i f v
    | 0 <= i && i < Unboxed.length v = f (v Unboxed.! i) <&> \a -> v Unboxed.// [(i, a)]
    | otherwise                      = pure v
  {-# INLINE ix #-}

type instance IxValue StrictT.Text = Char
instance Ixed StrictT.Text where
  ix e f s = case StrictT.splitAt e s of
     (l, mr) -> case StrictT.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> StrictT.concat [l, StrictT.singleton d, xs]
  {-# INLINE ix #-}

type instance IxValue LazyT.Text = Char
instance Ixed LazyT.Text where
  ix e f s = case LazyT.splitAt e s of
     (l, mr) -> case LazyT.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> LazyT.append l (LazyT.cons d xs)
  {-# INLINE ix #-}

type instance IxValue StrictB.ByteString = Word8
instance Ixed StrictB.ByteString where
  ix e f s = case StrictB.splitAt e s of
     (l, mr) -> case StrictB.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> StrictB.concat [l, StrictB.singleton d, xs]
  {-# INLINE ix #-}

type instance IxValue LazyB.ByteString = Word8
instance Ixed LazyB.ByteString where
  -- TODO: we could be lazier, returning each chunk as it is passed
  ix e f s = case LazyB.splitAt e s of
     (l, mr) -> case LazyB.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> LazyB.append l (LazyB.cons d xs)
  {-# INLINE ix #-}



-- | 'At' provides a 'Lens' that can be used to read,
-- write or delete the value associated with a key in a 'Map'-like
-- container on an ad hoc basis.
--
-- An instance of 'At' should satisfy:
--
-- @
-- 'ix' k ≡ 'at' k '.' 'traverse'
-- @
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
  at :: Index m -> Lens' m (Maybe (IxValue m))

-- | Delete the value associated with a key in a 'Map'-like container
--
-- @
-- 'sans' k = 'at' k .~ Nothing
-- @
sans :: At m => Index m -> m -> m
sans k m = m & at k .~ Nothing
{-# INLINE sans #-}

-- | An indexed version of 'at'.
--
-- >>> Map.fromList [(1,"world")] ^@. iat 1
-- (1,Just "world")
--
-- >>> iat 1 %@~ (\i x -> if odd i then Just "hello" else Nothing) $ Map.empty
-- fromList [(1,"hello")]
--
-- >>> iat 2 %@~ (\i x -> if odd i then Just "hello" else Nothing) $ Map.empty
-- fromList []
--
iat :: At m => Index m -> IndexedLens' (Index m) m (Maybe (IxValue m))
iat i f = at i (indexed f i)
{-# INLINE iat #-}

instance At (Maybe a) where
  at ~() f = f
  {-# INLINE at #-}

instance At (IntMap a) where
#if MIN_VERSION_containers(0,5,8)
  at k f = IntMap.alterF f k
#else
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (IntMap.delete k m)) mv
    Just v' -> IntMap.insert k v' m
    where mv = IntMap.lookup k m
#endif
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
#if MIN_VERSION_containers(0,5,8)
  at k f = Map.alterF f k
#else
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
#endif
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At (HashMap k a) where
  at k f = HashMap.alterF f k
  {-# INLINE at #-}

instance At IntSet where
  -- This is a gently modified copy of the implementation of `IntSet.alterF`.
  -- Unlike a `Set`, we don't have to worry about expensive comparisons from
  -- descending multiple times into an `IntSet`. We are careful to share the
  -- results of insertion or deletion across multiple positions in the
  -- `Functor`.
  at k f s = fmap choose (f (guard member_))
    where
      member_ = IntSet.member k s

      (inserted, deleted)
        | member_   = (s, IntSet.delete k s)
        | otherwise = (IntSet.insert k s, s)

      choose (Just ~()) = inserted
      choose Nothing = deleted
  {-# INLINE at #-}

instance Ord k => At (Set k) where
#if MIN_VERSION_containers(0,6,3)
  at k f = Set.alterF (fmap isJust . f . guard) k
#else
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Set.delete k m)) mv
    Just ~() -> maybe (Set.insert k m) (const m) mv
    where mv = if Set.member k m then Just () else Nothing
#endif
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At (HashSet k) where
  at k f s = HashSet.fromMap <$> HashMap.alterF f k (HashSet.toMap s)
  {-# INLINE at #-}


-- | @'ix' :: 'Int' -> 'Traversal'' (a,a) a@
type instance IxValue (a,a2) = a
instance (a~a2) => Ixed (a,a2) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a) a@
type instance IxValue (a,a2,a3) = a
instance (a~a2, a~a3) => Ixed (a,a2,a3) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a,a) a@
type instance IxValue (a,a2,a3,a4) = a
instance (a~a2, a~a3, a~a4) => Ixed (a,a2,a3,a4) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a,a,a) a@
type instance IxValue (a,a2,a3,a4,a5) = a
instance (a~a2, a~a3, a~a4, a~a5) => Ixed (a,a2,a3,a4,a5) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a,a,a,a) a@
type instance IxValue (a,a2,a3,a4,a5,a6) = a
instance (a~a2, a~a3, a~a4, a~a5, a~a6) => Ixed (a,a2,a3,a4,a5,a6) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a,a,a,a,a) a@
type instance IxValue (a,a2,a3,a4,a5,a6,a7) = a
instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7) => Ixed (a,a2,a3,a4,a5,a6,a7) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a,a,a,a,a,a) a@
type instance IxValue (a,a2,a3,a4,a5,a6,a7,a8) = a
instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8) => Ixed (a,a2,a3,a4,a5,a6,a7,a8) where
  ix p = elementOf each p

-- | @'ix' :: 'Int' -> 'Traversal'' (a,a,a,a,a,a,a,a,a) a@
type instance IxValue (a,a2,a3,a4,a5,a6,a7,a8,a9) = a
instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, a~a9) => Ixed (a,a2,a3,a4,a5,a6,a7,a8,a9) where
  ix p = elementOf each p
