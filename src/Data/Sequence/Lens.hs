{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Sequence.Lens
  ( ordinal
  , viewL, viewR
  , _head, _tail
  , _last, _init
  , sliced, slicedTo, slicedFrom
  ) where

import Control.Applicative
import Control.Lens as Lens
import Data.Monoid
import Data.Sequence as Seq

-- | A 'Lens' that can access the @n@th element of a 'Seq'.
--
-- Note: This is only a legal lens if there is already such an element!
ordinal :: Int -> SimpleIndexedLens Int (Seq a) a
ordinal i = Lens.index $ \ f m -> (\a -> update i a m) <$> f i (Seq.index m i)

-- * Sequence isomorphisms

-- | A 'Seq' is isomorphic to a 'ViewL'
--
-- @'viewl' m = m '^.' 'viewL'@
viewL :: Iso (Seq a) (Seq b) (ViewL a) (ViewL b)
viewL = isos viewl unviewl viewl unviewl where

unviewl :: ViewL a -> Seq a
unviewl EmptyL = mempty
unviewl (a :< as) = a <| as
{-# INLINE viewL #-}

-- | A 'Seq' is isomorphic to a 'ViewR'
--
-- @'viewr' m = m '^.' 'viewR'@
viewR :: Iso (Seq a) (Seq b) (ViewR a) (ViewR b)
viewR = isos viewr unviewr viewr unviewr where
{-# INLINE viewR #-}

unviewr :: ViewR a -> Seq a
unviewr EmptyR = mempty
unviewr (as :> a) = as |> a

-- * Traversals

-- | Traverse the head of a 'Seq'
_head :: SimpleIndexedTraversal Int (Seq a) a
_head = Lens.index $ \f m -> case viewl m of
  a :< as -> (<| as) <$> f (0::Int) a
  EmptyL  -> pure m
{-# INLINE _head #-}

-- | Traverse the tail of a 'Seq'
_tail :: SimpleTraversal (Seq a) (Seq a)
_tail f m = case viewl m of
  a :< as -> (a <|) <$> f as
  EmptyL  -> pure m
{-# INLINE _tail #-}

-- | Traverse the last element of a 'Seq'
_last :: SimpleIndexedTraversal Int (Seq a) a
_last = Lens.index $ \f m ->  case viewr m of
  as :> a -> (as |>) <$> f (Seq.length as) a
  EmptyR  -> pure m
{-# INLINE _last #-}

-- | Traverse all but the last element of a 'Seq'
_init :: SimpleTraversal (Seq a) (Seq a)
_init f m = case viewr m of
  as :> a -> (|> a) <$> f as
  EmptyR  -> pure m
{-# INLINE _init #-}

-- | Traverse the first @n@ elements of a 'Seq'
slicedTo :: Int -> SimpleIndexedTraversal Int (Seq a) a
slicedTo n = Lens.index $ \f m -> case Seq.splitAt n m of
  (l,r) -> (>< r) <$> itraverse f l
{-# INLINE slicedTo #-}

-- | Traverse all but the first @n@ elements of a 'Seq'
slicedFrom :: Int -> SimpleIndexedTraversal Int (Seq a) a
slicedFrom n = Lens.index $ \ f m -> case Seq.splitAt n m of
  (l,r) -> (l ><) <$> itraverse (f . (+n)) r
{-# INLINE slicedFrom #-}

-- | Travere all the elements numbered from @i@ to @j@ of a 'Seq'
sliced :: Int -> Int -> SimpleIndexedTraversal Int (Seq a) a
sliced i j = Lens.index $ \ f s -> case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> (\n -> l >< n >< r) <$> itraverse (f . (+i)) m
{-# INLINE sliced #-}
