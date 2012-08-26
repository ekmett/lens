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
  ( ordinal, viewL, viewR
  , traverseHead, traverseTail
  , traverseLast, traverseInit
  , traverseTo, traverseFrom
  , traverseSlice
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

traverseSeq :: IndexedTraversal Int (Seq a) (Seq b) a b
traverseSeq = indexed traverse
{-# INLINE traverseSeq #-}

-- * Traversals

-- | Traverse the head of a 'Seq'
traverseHead :: SimpleIndexedTraversal Int (Seq a) a
traverseHead = Lens.index $ \f m -> case viewl m of
  a :< as -> (<| as) <$> f (0::Int) a
  EmptyL  -> pure m
{-# INLINE traverseHead #-}

-- | Traverse the tail of a 'Seq'
traverseTail :: SimpleIndexedTraversal Int (Seq a) a
traverseTail = Lens.index $ \f m -> case viewl m of
  a :< as -> (a <|) <$> withIndex traverseSeq (f . (+1)) as
  EmptyL  -> pure m
{-# INLINE traverseTail #-}

-- | Traverse the last element of a 'Seq'
traverseLast :: SimpleIndexedTraversal Int (Seq a) a
traverseLast = Lens.index $ \f m ->  case viewr m of
  as :> a -> (as |>) <$> f (Seq.length as) a
  EmptyR  -> pure m
{-# INLINE traverseLast #-}

-- | Traverse all but the last element of a 'Seq'
traverseInit :: SimpleIndexedTraversal Int (Seq a) a
traverseInit = Lens.index $ \ f m -> case viewr m of
  as :> a -> (|> a) <$> withIndex traverseSeq f as
  EmptyR  -> pure m
{-# INLINE traverseInit #-}

-- | Traverse the first @n@ elements of a 'Seq'
traverseTo :: Int -> SimpleIndexedTraversal Int (Seq a) a
traverseTo n = Lens.index $ \f m -> case Seq.splitAt n m of
  (l,r) -> (>< r) <$> withIndex traverseSeq f l
{-# INLINE traverseTo #-}

-- | Traverse all but the first @n@ elements of a 'Seq'
traverseFrom :: Int -> SimpleIndexedTraversal Int (Seq a) a
traverseFrom n = Lens.index $ \ f m -> case Seq.splitAt n m of
  (l,r) -> (l ><) <$> withIndex traverseSeq (f . (+n)) r
{-# INLINE traverseFrom #-}

-- | Travere all the elements numbered from @i@ to @j@ of a 'Seq'
traverseSlice :: Int -> Int -> SimpleIndexedTraversal Int (Seq a) a
traverseSlice i j = Lens.index $ \ f s -> case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> (\n -> l >< n >< r) <$> withIndex traverseSeq (f . (+i)) m
{-# INLINE traverseSlice #-}
