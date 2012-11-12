{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Generic.Lens
  ( toVectorOf
  -- * Isomorphisms
  , forced
  , vector
  , asStream
  , asStreamR
  , cloned
  , reversed
  -- * Lenses
  , _head
  , _tail
  , _last
  , _init
  , sliced
  -- * Traversal of individual indices
  , atIndex
  , atIndices
  -- * Convenience combinators for modification
  , (++~), (<++~)
  , (++=), (<++=)
  , (///~), (<///~)
  , (///=), (<///=)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State as State (MonadState, modify)
import Data.Vector.Generic as V hiding (zip, filter)
import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)
import Prelude hiding ((++), length, head, tail, init, last, map, reverse)

infixr 4 ++~, <++~, ///~, <///~
infix 4 ++=, <++=, ///=, <///=

-- | A lens reading and writing to the 'head' of a /non-empty/ 'Vector'
--
-- Attempting to read or write to the 'head' of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> V.fromList [1,2,3]^._head
-- 1
_head :: Vector v a => SimpleLens (v a) a
_head f v = (\a -> v // [(0,a)]) <$> f (head v)
{-# INLINE _head #-}

-- | A 'Lens' reading and writing to the 'last' element of a /non-empty/ 'Vector'
--
-- Attempting to read or write to the 'last' element of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> V.fromList [1,2]^._last
-- 2
_last :: Vector v a => SimpleLens (v a) a
_last f v = (\a -> v // [(length v - 1, a)]) <$> f (last v)
{-# INLINE _last #-}

-- | A lens reading and writing to the 'tail' of a /non-empty/ 'Vector'
--
-- Attempting to read or write to the 'tail' of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> _tail .~ V.fromList [3,4,5] $ V.fromList [1,2]
-- fromList [1,3,4,5]
_tail :: Vector v a => SimpleLens (v a) (v a)
_tail f v = cons (head v) <$> f (tail v)
{-# INLINE _tail #-}

-- | A 'Lens' reading and replacing all but the a 'last' element of a /non-empty/ 'Vector'
--
-- Attempting to read or write to all but the 'last' element of an /empty/ 'Vector' will result in an 'error'.
--
-- >>> V.fromList [1,2,3,4]^._init
-- [1,2,3]
_init :: Vector v a => SimpleLens (v a) (v a)
_init f v = (`snoc` last v) <$> f (init v)
{-# INLINE _init #-}

-- | @sliced i n@ provides a lens that edits the @n@ elements starting at index @i@ from a lens.
--
-- This is only a valid lens if you do not change the length of the resulting 'Vector'.
--
-- Attempting to return a longer or shorter vector will result in violations of the 'Lens' laws.
sliced :: Vector v a => Int -- ^ @i@ starting index
          -> Int -- ^ @n@ length
          -> SimpleLens (v a) (v a)
sliced i n f v = (\ v0 -> v // zip [i..i+n-1] (V.toList v0)) <$> f (slice i n v)
{-# INLINE sliced #-}

-- Similar to 'toListOf', but returning a 'Vector'.
toVectorOf :: Vector v a => Getting [a] s t a b -> s -> v a
toVectorOf l s = fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Append to the target(s) of a 'Vector'-valued 'Setter'.
(++~) :: Vector v a => Setting s t (v a) (v a) -> v a -> s -> t
v ++~ n = over v (++ n)
{-# INLINE (++~) #-}

-- | Append to the target(s) of a 'Vector'-valued 'Setter' in the current monadic state.
(++=) :: (MonadState s m, Vector v a) => SimpleSetting s (v a) -> v a -> m ()
v ++= b = State.modify (v ++~ b)
{-# INLINE (++=) #-}

-- | Append to the target of a 'Vector'-valued 'Lens', returning the result as well as the updated structure.
(<++~) :: Vector v a => LensLike ((,) (v a)) s t (v a) (v a) -> v a -> s -> (v a, t)
v <++~ m = v <%~ (++ m)
{-# INLINE (<++~) #-}

-- | Append to the target of a 'Vector'-valued 'Lens' in the current monadic state, returning the result.
(<++=) :: (MonadState s m, Vector v a) => SimpleLensLike ((,) (v a)) s (v a) -> v a -> m (v a)
v <++= m = v <%= (++ m)
{-# INLINE (<++=) #-}

vector :: Vector v a => Simple Iso [a] (v a)
vector = iso fromList V.toList
{-# INLINE vector #-}

asStream :: Vector v a => Simple Iso (v a) (Stream a)
asStream = iso stream unstream
{-# INLINE asStream #-}

asStreamR :: Vector v a => Simple Iso (v a) (Stream a)
asStreamR = iso streamR unstreamR
{-# INLINE asStreamR #-}

cloned :: Vector v a => Simple Iso (v a) (New v a)
cloned = iso clone new
{-# INLINE cloned #-}

forced :: Vector v a => Simple Iso (v a) (v a)
forced = iso force force
{-# INLINE forced #-}

(///~) :: Vector v a => Setting s t (v a) (v a) -> [(Int, a)] -> s -> t
v ///~ n = over v (// n)
{-# INLINE (///~) #-}

(///=) :: (MonadState s m, Vector v a) => SimpleSetting s (v a) -> [(Int, a)] -> m ()
v ///= b = State.modify (v ///~ b)
{-# INLINE (///=) #-}

(<///~) :: Vector v a => LensLike ((,)(v a)) s t (v a) (v a) -> [(Int, a)] -> s -> (v a, t)
v <///~ m = v <%~ (// m)
{-# INLINE (<///~) #-}

(<///=) :: (MonadState s m, Vector v a) => SimpleLensLike ((,)(v a)) s (v a) -> [(Int, a)] -> m (v a)
v <///= m = v <%= (// m)
{-# INLINE (<///=) #-}

-- | This is a more efficient version of 'element' that works for any 'Vector'.
--
-- @atIndex n@ is only a valid 'Lens' into a 'Vector' with 'length' at least @n + 1@.
atIndex :: Vector v a => Int -> SimpleIndexedLens Int (v a) a
atIndex i = index $ \ f v -> (\ a -> v // [(i, a)]) <$> f i (v ! i)
{-# INLINE atIndex #-}

-- | This is only a valid 'Traversal' if the supplied list of indices contains no duplicates.
atIndices :: Vector v a => [Int] -> SimpleIndexedTraversal Int (v a) a
atIndices is = index $ \ f v -> let
     l = length v
     is' = filter (<l) is
  in fmap ((v //) . zip is') . traverse (uncurry f) . zip is $ fmap (v !) is'
{-# INLINE atIndices #-}
