{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.At
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.At
  (
  -- * Contains
    Contains(..)
  , containsIx, containsAt, containsLength, containsN, containsTest, containsLookup
  -- * Ixed
  , Value
  , Ixed(ix)
  , ixAt, ixEach
  -- * At
  , At(at)
  -- * Deprecated
  , _at
  , resultAt
  ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Each
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed as Lens
import Control.Lens.Type
import Control.Lens.Traversal
import Data.Array.IArray as Array
import Data.Array.Unboxed
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Complex
import Data.Functor.Identity
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap hiding (Key)
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set as Set
import Data.Sequence as Seq
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Tree
import Data.Vector as Vector hiding (indexed)
import Data.Vector.Primitive as Prim
import Data.Vector.Storable as Storable
import Data.Vector.Unboxed as Unboxed
import Data.Word

-- $setup
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- | A deprecated alias for 'ix'.
_at, resultAt :: Ixed f m => Key m -> IndexedLensLike' (Key m) f m (Value m)
_at      = ix
resultAt = ix
{-# DEPRECATED _at, resultAt "use 'ix'. This function will be removed in version 3.9" #-}

class Functor f => Contains f m where
  -- |
  -- This simple 'IndexedTraversal' lets you view (and sometimes modify) whether
  -- or not a map (or set) contains a given key.
  --
  -- >>> IntSet.fromList [1,2,3,4] ^. contains 3
  -- True
  --
  -- >>> IntSet.fromList [1,2,3,4] ^. contains 5
  -- False
  --
  -- >>> IntSet.fromList [1,2,3,4] & contains 3 .~ False
  -- fromList [1,2,4]
  contains :: Key m -> IndexedLensLike' (Key m) f m Bool
#ifdef DEFAULT_SIGNATURES
  default contains :: (Gettable f, At m) => Key m -> IndexedLensLike' (Key m) f m Bool
  contains = containsAt
#endif

-- | A definition of 'ix' for types with an 'At' instance. This is the default
-- if you don't specify a definition for 'ix'.
containsIx :: (Gettable f, Ixed (Accessor Any) m) => Key m -> IndexedLensLike' (Key m) f m Bool
containsIx i f = coerce . Lens.indexed f i . has (ix i)
{-# INLINE containsIx #-}

-- | A definition of 'ix' for types with an 'At' instance. This is the default
-- if you don't specify a definition for 'ix'.
containsAt :: (Gettable f, At m) => Key m -> IndexedLensLike' (Key m) f m Bool
containsAt i f = coerce . Lens.indexed f i . views (at i) isJust
{-# INLINE containsAt #-}

containsLength :: forall i s. (Ord i, Num i) => (s -> i) -> i -> IndexedGetter i s Bool
containsLength sn = \ i pafb s -> coerce $ Lens.indexed pafb (i :: i) (0 <= i && i < sn s)
{-# INLINE containsLength #-}

containsN :: Int -> Int -> IndexedGetter Int s Bool
containsN n = \ i pafb _ -> coerce $ Lens.indexed pafb (i :: Int) (0 <= i && i < n)
{-# INLINE containsN #-}

containsTest :: forall i s. (i -> s -> Bool) -> i -> IndexedGetter i s Bool
containsTest isb = \i pafb s -> coerce $ Lens.indexed pafb (i :: i) (isb i s)
{-# INLINE containsTest #-}

containsLookup :: forall i s a. (i -> s -> Maybe a) -> i -> IndexedGetter i s Bool
containsLookup isb = \i pafb s -> coerce $ Lens.indexed pafb (i :: i) (isJust (isb i s))
{-# INLINE containsLookup #-}

instance Gettable f => Contains f (e -> a) where
  contains i f _ = coerce (Lens.indexed f i True)
  {-# INLINE contains #-}

instance Functor f => Contains f IntSet where
  contains k f s = Lens.indexed f k (IntSet.member k s) <&> \b ->
    if b then IntSet.insert k s else IntSet.delete k s
  {-# INLINE contains #-}

instance (Functor f, Ord a) => Contains f (Set a) where
  contains k f s = Lens.indexed f k (Set.member k s) <&> \b ->
    if b then Set.insert k s else Set.delete k s
  {-# INLINE contains #-}

instance (Functor f, Eq a, Hashable a) => Contains f (HashSet a) where
  contains k f s = Lens.indexed f k (HashSet.member k s) <&> \b ->
    if b then HashSet.insert k s else HashSet.delete k s
  {-# INLINE contains #-}

instance Gettable f => Contains f [a] where
  contains = containsLength Prelude.length
  {-# INLINE contains #-}

instance Gettable f => Contains f (Seq a) where
  contains = containsLength Seq.length
  {-# INLINE contains #-}

#if MIN_VERSION_base(4,4,0)
instance Gettable f => Contains f (Complex a) where
  contains = containsN 2
#else
instance (Gettable f, RealFloat a) => Contains f (Complex a) where
  contains = containsN 2
#endif

-- | @'each' :: 'IndexedTraversal' ['Int'] ('Tree' a) ('Tree' b) a b@
instance Gettable f => Contains f (Tree a) where
  contains xs0 pafb = coerce . Lens.indexed pafb xs0 . go xs0 where
    go [] (Node _ _) = True
    go (i:is) (Node _ as) = goto i is as where
    goto 0 is (a:_) = go is a
    goto _ _  []     = False
    goto n is (_:as) = (goto $! n - 1) is as

instance Gettable k => Contains k (Identity a) where
  contains () f _ = coerce (Lens.indexed f () True)

instance Gettable k => Contains k (a,b) where
  contains = containsN 2
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c) where
  contains = containsN 3
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c,d) where
  contains = containsN 4
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c,d,e) where
  contains = containsN 5
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c,d,e,f) where
  contains = containsN 6
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c,d,e,f,g) where
  contains = containsN 7
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c,d,e,f,g,h) where
  contains = containsN 8
  {-# INLINE contains #-}

instance Gettable k => Contains k (a,b,c,d,e,f,g,h,i) where
  contains = containsN 9
  {-# INLINE contains #-}

instance Gettable k => Contains k (IntMap a) where
  contains = containsLookup IntMap.lookup
  {-# INLINE contains #-}

instance (Gettable f, Ord k) => Contains f (Map k a) where
  contains = containsLookup Map.lookup
  {-# INLINE contains #-}

instance (Gettable f, Eq k, Hashable k) => Contains f (HashMap k a) where
  contains = containsLookup HashMap.lookup
  {-# INLINE contains #-}

instance (Gettable f, Ix i) => Contains f (Array i e) where
  contains = containsTest $ \i s -> inRange (bounds s) i
  {-# INLINE contains #-}

instance (Gettable f, IArray UArray e, Ix i) => Contains f (UArray i e) where
  contains = containsTest $ \i s -> inRange (bounds s) i
  {-# INLINE contains #-}

instance Gettable f => Contains f (Vector.Vector a) where
  contains = containsLength Vector.length
  {-# INLINE contains #-}

instance (Gettable f, Prim a) => Contains f (Prim.Vector a) where
  contains = containsLength Prim.length
  {-# INLINE contains #-}

instance (Gettable f, Storable a) => Contains f (Storable.Vector a) where
  contains = containsLength Storable.length
  {-# INLINE contains #-}

instance (Gettable f, Unbox a) => Contains f (Unboxed.Vector a) where
  contains = containsLength Unboxed.length
  {-# INLINE contains #-}

instance Gettable f => Contains f StrictT.Text where
  contains = containsTest $ \i s -> StrictT.compareLength s i == GT
  {-# INLINE contains #-}

instance Gettable f => Contains f LazyT.Text where
  contains = containsTest $ \i s -> LazyT.compareLength s i == GT
  {-# INLINE contains #-}

instance Gettable f => Contains f StrictB.ByteString where
  contains = containsLength StrictB.length
  {-# INLINE contains #-}

instance Gettable f => Contains f LazyB.ByteString where
  contains = containsTest $ \i s -> not (LazyB.null (LazyB.drop i s))
  {-# INLINE contains #-}

type family Value (m :: *) :: *

-- | This simple 'IndexedTraversal' lets you 'traverse' the value at a given
-- key in a 'Map' or element at an ordinal position in a list or 'Seq'.
class Contains (Accessor (Value m)) m => Ixed f m where
  -- | This simple 'IndexedTraversal' lets you 'traverse' the value at a given
  -- key in a 'Map' or element at an ordinal position in a list or 'Seq'.
  --
  -- /NB:/ Setting the value of this 'Traversal' will only set the value in the
  -- 'Lens' if it is already present.
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
  ix :: Key m -> IndexedLensLike' (Key m) f m (Value m)
#ifdef DEFAULT_SIGNATURES
  default ix :: (Applicative f, At m) => Key m -> IndexedLensLike' (Key m) f m (Value m)
  ix = ixAt
#endif

-- | A definition of 'ix' for types with an 'At' instance. This is the default
-- if you don't specify a definition for 'ix'.
ixAt :: (Applicative f, At m) => Key m -> IndexedLensLike' (Key m) f m (Value m)
ixAt i = at i <. traverse
{-# INLINE ixAt #-}

-- | A definition of 'ix' for types with an 'Each' instance.
ixEach :: (Applicative f, Eq (Key m), Each f m m (Value m) (Value m)) => Key m -> IndexedLensLike' (Key m) f m (Value m)
ixEach i = each . Lens.index i
{-# INLINE ixEach #-}

type instance Value [a] = a
instance Applicative f => Ixed f [a] where
  ix k f xs0 = go xs0 k where
    go [] _ = pure []
    go (a:as) 0 = Lens.indexed f k a <&> (:as)
    go (a:as) i = (a:) <$> (go as $! i - 1)
  {-# INLINE ix #-}

type instance Value (Identity a) = a
instance Functor f => Ixed f (Identity a) where
  ix () f (Identity a) = Identity <$> Lens.indexed f () a
  {-# INLINE ix #-}

type instance Value (Tree a) = a
instance Applicative f => Ixed f (Tree a) where
  ix xs0 f = go xs0 where
    go [] (Node a as) = Lens.indexed f xs0 a <&> \a' -> Node a' as
    go (i:is) (Node a as) = Node a <$> goto is as i
    goto is (a:as) 0 = go is a <&> (:as)
    goto is (_:as) n = goto is as $! n - 1
    goto _  []     _ = pure []
  {-# INLINE ix #-}

type instance Value (Seq a) = a
instance Applicative f => Ixed f (Seq a) where
  ix i f m
    | 0 <= i && i < Seq.length m = Lens.indexed f i (Seq.index m i) <&> \a -> Seq.update i a m
    | otherwise                  = pure m
  {-# INLINE ix #-}

type instance Value (IntMap a) = a
instance Applicative f => Ixed f (IntMap a) where
  ix k f m = case IntMap.lookup k m of
     Just v -> Lens.indexed f k v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

type instance Value (Map k a) = a
instance (Applicative f, Ord k) => Ixed f (Map k a) where
  ix k f m = case Map.lookup k m of
     Just v  -> Lens.indexed f k v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

type instance Value (HashMap k a) = a
instance (Applicative f, Eq k, Hashable k) => Ixed f (HashMap k a) where
  ix k f m = case HashMap.lookup k m of
     Just v  -> Lens.indexed f k v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

type instance Value (Array i e) = e
-- |
-- @
-- arr '!' i ≡ arr 'Control.Lens.Getter.^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i 'Control.Lens.Setter..~' e '$' arr
-- @
instance (Applicative f, Ix i) => Ixed f (Array i e) where
  ix i f arr
    | inRange (bounds arr) i = Lens.indexed f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

type instance Value (UArray i e) = e
-- |
-- @
-- arr '!' i ≡ arr 'Control.Lens.Getter.^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i 'Control.Lens.Setter..~' e '$' arr
-- @
instance (Applicative f, IArray UArray e, Ix i) => Ixed f (UArray i e) where
  ix i f arr
    | inRange (bounds arr) i = Lens.indexed f i (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

type instance Value (Vector.Vector a) = a
instance Applicative f => Ixed f (Vector.Vector a) where
  ix i f v
    | 0 <= i && i < Vector.length v = Lens.indexed f i (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    | otherwise                     = pure v
  {-# INLINE ix #-}

type instance Value (Prim.Vector a) = a
instance (Applicative f, Prim a) => Ixed f (Prim.Vector a) where
  ix i f v
    | 0 <= i && i < Prim.length v = Lens.indexed f i (v Prim.! i) <&> \a -> v Prim.// [(i, a)]
    | otherwise                   = pure v
  {-# INLINE ix #-}

type instance Value (Storable.Vector a) = a
instance (Applicative f, Storable a) => Ixed f (Storable.Vector a) where
  ix i f v
    | 0 <= i && i < Storable.length v = Lens.indexed f i (v Storable.! i) <&> \a -> v Storable.// [(i, a)]
    | otherwise                       = pure v
  {-# INLINE ix #-}

type instance Value (Unboxed.Vector a) = a
instance (Applicative f, Unbox a) => Ixed f (Unboxed.Vector a) where
  ix i f v
    | 0 <= i && i < Unboxed.length v = Lens.indexed f i (v Unboxed.! i) <&> \a -> v Unboxed.// [(i, a)]
    | otherwise                      = pure v
  {-# INLINE ix #-}

type instance Value StrictT.Text = Char
instance Applicative f => Ixed f StrictT.Text where
  ix e f s = case StrictT.splitAt e s of
     (l, mr) -> case StrictT.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> Lens.indexed f e c <&> \d -> StrictT.concat [l, StrictT.singleton d, xs]
  {-# INLINE ix #-}

type instance Value LazyT.Text = Char
instance Applicative f => Ixed f LazyT.Text where
  ix e f s = case LazyT.splitAt e s of
     (l, mr) -> case LazyT.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> Lens.indexed f e c <&> \d -> LazyT.append l (LazyT.cons d xs)
  {-# INLINE ix #-}

type instance Value StrictB.ByteString = Word8
instance Applicative f => Ixed f StrictB.ByteString where
  ix e f s = case StrictB.splitAt e s of
     (l, mr) -> case StrictB.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> Lens.indexed f e c <&> \d -> StrictB.concat [l, StrictB.singleton d, xs]
  {-# INLINE ix #-}

type instance Value LazyB.ByteString = Word8
instance Applicative f => Ixed f LazyB.ByteString where
  -- TODO: we could be lazier, returning each chunk as it is passed
  ix e f s = case LazyB.splitAt e s of
     (l, mr) -> case LazyB.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> Lens.indexed f e c <&> \d -> LazyB.append l (LazyB.cons d xs)
  {-# INLINE ix #-}


type instance Value (k -> a) = a
instance (Functor f, Eq k) => Ixed f (k -> a) where
  ix e g f = Lens.indexed g e (f e) <&> \a' e' -> if e == e' then a' else f e'
  {-# INLINE ix #-}

type instance Value (a,a) = a
instance (Applicative f, a ~ b) => Ixed f (a,b) where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a) = a
instance (Applicative f, a ~ b, b ~ c) => Ixed f (a,b,c) where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a,a) = a
instance (Applicative f, a ~ b, b ~ c, c ~ d) => Ixed f (a,b,c,d) where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a,a,a) = a
instance (Applicative f, a ~ b, b ~ c, c ~ d, d ~ e) => Ixed f (a,b,c,d,e) where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a,a,a,a) = a
instance (Applicative f, a ~ b, b ~ c, c ~ d, d ~ e, e ~ f') => Ixed f (a,b,c,d,e,f') where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a,a,a,a,a) = a
instance (Applicative f, a ~ b, b ~ c, c ~ d, d ~ e, e ~ f', f' ~ g) => Ixed f (a,b,c,d,e,f',g) where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a,a,a,a,a,a) = a
instance (Applicative f, a ~ b, b ~ c, c ~ d, d ~ e, e ~ f', f' ~ g, g ~ h) => Ixed f (a,b,c,d,e,f',g,h) where
  ix = ixEach
  {-# INLINE ix #-}

type instance Value (a,a,a,a,a,a,a,a,a) = a
instance (Applicative f, a ~ b, b ~ c, c ~ d, d ~ e, e ~ f', f' ~ g, g ~ h, h ~ i) => Ixed f (a,b,c,d,e,f',g,h,i) where
  ix = ixEach
  {-# INLINE ix #-}

-- | 'At' provides a 'Lens' that can be used to read,
-- write or delete the value associated with a key in a 'Map'-like
-- container on an ad hoc basis.
--
-- An instance of 'At' should satisfy:
--
-- @'ix' k ≡ 'at' k '<.' 'traverse'@
class At m where
  -- |
  -- >>> Map.fromList [(1,"world")] ^.at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at :: Key m -> IndexedLens' (Key m) m (Maybe (Value m))

instance At (IntMap a) where
  at k f m = Lens.indexed f k mv <&> \r -> case r of
    Nothing -> maybe m (const (IntMap.delete k m)) mv
    Just v' -> IntMap.insert k v' m
    where mv = IntMap.lookup k m
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
  at k f m = Lens.indexed f k mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At (HashMap k a) where
  at k f m = Lens.indexed f k mv <&> \r -> case r of
    Nothing -> maybe m (const (HashMap.delete k m)) mv
    Just v' -> HashMap.insert k v' m
    where mv = HashMap.lookup k m
  {-# INLINE at #-}
