{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Lens
  ( Vector
  , toVectorOf
  -- * Isomorphisms
  , reversed
  , zippedWithIndex
  , forced
  , asList
  , asStream
  , asStreamR
  , converted
  , cloned
  -- * Lenses
  , _head
  , _tail
  , _last
  , _init
  , atIndex
  , sliced
  , taken
  , dropped
  , takenWhile
  , droppedWhile
  -- * Traversals
  , traverseVector
  , atIndices
  -- * Getters
  , backpermuted
  -- * Convenience combinators for modification
  , (++~), (<++~)
  , (++=), (<++=)
  , (///~), (<///~)
  , (///=), (<///=)
  ) where

import Control.Applicative
import Data.Traversable (traverse)
import Control.Lens
import Control.Lens.Internal (coerce)
import Data.List.Lens (traverseList)
import Control.Monad.State as State (MonadState, modify)
import Control.Monad.ST (ST)
import Data.Vector.Generic (Vector, Mutable)
import qualified Data.Vector.Generic as V
import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)

infixr 4 ++~, <++~, ///~, <///~
infix 4 ++=, <++=, ///=, <///=

_head :: (Vector v a) => SimpleIndexedLens Int (v a) a
_head = index $ \ f v -> flip V.cons (V.tail v) <$> f (0::Int) (V.head v)
{-# INLINE _head #-}

_last :: (Vector v a) => SimpleIndexedLens Int (v a) a
_last = index $ \ f v -> V.snoc (V.init v) <$> f (V.length v - (1::Int)) (V.last v)
{-# INLINE _last #-}

_tail :: (Vector v a) => SimpleIndexedLens (Int, Int) (v a) (v a)
_tail = index $ \ f v -> V.cons (V.head v) <$> f ((1::Int), V.length v - (1::Int)) (V.tail v)
{-# INLINE _tail #-}

_init :: (Vector v a) => SimpleIndexedLens (Int, Int) (v a) (v a)
_init = index $ \ f v -> flip V.snoc (V.last v) <$> f ((0::Int), V.length v - (2::Int)) (V.init v)
{-# INLINE _init #-}

atIndex :: (Vector v a) => Int -> SimpleIndexedLens Int (v a) a
atIndex i = index $ \ f v -> (\ a -> v V.// [(i, a)]) <$> f i (v V.! i)
{-# INLINE atIndex #-}

sliced :: (Vector v a) => Int -- ^ @i@ starting index
          -> Int -- ^ @n@ length
          -> SimpleIndexedLens (Int, Int) (v a) (v a)
sliced i n = index $ \ f v -> (\ v0 -> v V.// zip [i..(i+n-(1::Int))] (V.toList v0)) <$> f (i, i+n-(1::Int)) (V.slice i n v)
{-# INLINE sliced #-}

reversed :: (Vector v a, Vector v b) => Iso (v a) (v b) (v a) (v b)
reversed = isos V.reverse V.reverse V.reverse V.reverse
{-# INLINE reversed #-}

converted :: (Vector v1 a, Vector v1 b, Vector v2 a, Vector v2 b) => Iso (v1 a) (v1 b) (v2 a) (v2 b)
converted = isos V.convert V.convert V.convert V.convert
{-# INLINE converted #-}

toVectorOf :: (Vector v a) => Getting (v a) s t a b -> s -> v a
toVectorOf = flip foldMapOf V.singleton
{-# INLINE toVectorOf #-}

(++~) :: (Vector v a) => Setting s t (v a) (v a) -> v a -> s -> t
v ++~ n = over v (V.++ n)
{-# INLINE (++~) #-}

(++=) :: (Vector v a, MonadState s m) => SimpleSetting s (v a) -> v a -> m ()
v ++= b = State.modify (v ++~ b)
{-# INLINE (++=) #-}

(<++~) :: (Vector v a) => LensLike ((,)(v a)) s t (v a) (v a) -> v a -> s -> (v a, t)
v <++~ m = v <%~ (V.++ m)
{-# INLINE (<++~) #-}

(<++=) :: (Vector v a, MonadState s m) => SimpleLensLike ((,)(v a)) s (v a) -> v a -> m (v a)
v <++= m = v <%= (V.++ m)
{-# INLINE (<++=) #-}

backpermuted :: (Vector v a, Vector v Int) => v Int -- ^ @is@ index vector (of length @n@)
                -> IndexedGetter (v Int) (v a) (v a)
backpermuted is = index $ \ f -> coerce . f is . flip V.backpermute is
{-# INLINE backpermuted #-}

zippedWithIndex :: (Vector v a, Vector v (Int, a), Vector v b, Vector v (Int, b)) => Iso (v a) (v b) (v (Int, a)) (v (Int, b))
zippedWithIndex = isos V.indexed (V.map snd) V.indexed (V.map snd)
{-# INLINE zippedWithIndex #-}

taken :: (Vector v a) => Int -> SimpleIndexedLens (Int, Int) (v a) (v a)
taken i = index $ \ f v -> let (v0, v1) = V.splitAt i v in (V.++ v1) <$> f ((0::Int), i) v0
{-# INLINE taken #-}

dropped :: (Vector v a) => Int -> SimpleIndexedLens (Int, Int) (v a) (v a)
dropped i = index $ \ f v -> let (v0, v1) = V.splitAt i v in (v0 V.++) <$> f (i + (1::Int), V.length v - (1::Int)) v1
{-# INLINE dropped #-}

takenWhile :: (Vector v a) => (a -> Bool) -> SimpleIndexedLens (Int, Int) (v a) (v a)
takenWhile p = index $ \ f v -> let (v0, v1) = V.span p v in (V.++ v1) <$> f ((0::Int), V.length v0 - (1::Int)) v0
{-# INLINE takenWhile #-}

droppedWhile :: (Vector v a) => (a -> Bool) -> SimpleIndexedLens (Int, Int) (v a) (v a)
droppedWhile p = index $ \ f v -> let (v0, v1) = V.span p v in (v0 V.++) <$> f (V.length v0, V.length v - (1::Int)) v1
{-# INLINE droppedWhile #-}

asList :: (Vector v a, Vector v b) => Iso (v a) (v b) [a] [b]
asList = isos V.toList V.fromList V.toList V.fromList
{-# INLINE asList #-}

asStream :: (Vector v a, Vector v b) => Iso (v a) (v b) (Stream a) (Stream b)
asStream = isos V.stream V.unstream V.stream V.unstream
{-# INLINE asStream #-}

asStreamR :: (Vector v a, Vector v b) => Iso (v a) (v b) (Stream a) (Stream b)
asStreamR = isos V.streamR V.unstreamR V.streamR V.unstreamR
{-# INLINE asStreamR #-}

cloned :: (Vector v a, Vector v b) => Iso (v a) (v b) (New v a) (New v b)
cloned = isos V.clone V.new V.clone V.new
{-# INLINE cloned #-}

forced :: (Vector v a, Vector v b) => Iso (v a) (v b) (v a) (v b)
forced = isos V.force V.force V.force V.force
{-# INLINE forced #-}

atIndices :: (Vector v a) => [Int] -> SimpleIndexedTraversal Int (v a) a
atIndices is = index $ \ f v -> fmap ((v V.//) . zip is) . traverse (uncurry f) . zip is $ map (v V.!) is
{-# INLINE atIndices #-}

traverseVector :: (Vector v a, Vector v b) => IndexedTraversal Int (v a) (v b) a b
traverseVector = indexed $ asList .> traverseList
{-# INLINE traverseVector #-}

(///~) :: (Vector v a) => Setting s t (v a) (v a) -> [(Int, a)] -> s -> t
v ///~ n = over v (V.// n)
{-# INLINE (///~) #-}

(///=) :: (Vector v a, MonadState s m) => SimpleSetting s (v a) -> [(Int, a)] -> m ()
v ///= b = State.modify (v ///~ b)
{-# INLINE (///=) #-}

(<///~) :: (Vector v a) => LensLike ((,)(v a)) s t (v a) (v a) -> [(Int, a)] -> s -> (v a, t)
v <///~ m = v <%~ (V.// m)
{-# INLINE (<///~) #-}

(<///=) :: (Vector v a, MonadState s m) => SimpleLensLike ((,)(v a)) s (v a) -> [(Int, a)] -> m (v a)
v <///= m = v <%= (V.// m)
{-# INLINE (<///=) #-}
