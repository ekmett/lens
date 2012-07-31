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
  ( at, viewL, viewR
  , traverseHead, traverseTail
  , traverseLast, traverseInit
  , traverseTo, traverseFrom
  , traverseSlice
  ) where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Sequence as Seq

-- | A 'Lens' that can access the @n@th element of a 'Seq'.
--
-- Note: This is only a legal lens if there is such an element!
--
at :: Int -> Simple Lens (Seq a) a
at i f m = (\a -> update i a m) <$> f (index m i)

-- * Sequence isomorphisms

-- | A 'Seq' is isomorphic to a 'ViewL'
--
-- > viewl m = m^.viewL
viewL :: Iso (Seq a) (ViewL a)
viewL = iso viewl go where
  go EmptyL = mempty
  go (a :< as) = a <| as
{-# INLINE viewL #-}

-- | A 'Seq' is isomorphic to a 'ViewR'
--
-- > viewr m = m^.viewR
viewR :: Iso (Seq a) (ViewR a)
viewR = iso viewr go where
  go EmptyR = mempty
  go (as :> a) = as |> a
{-# INLINE viewR #-}

-- * Traversals

-- | Traverse the head of a 'Seq'
traverseHead :: Simple Traversal (Seq a) a
traverseHead f m = case viewl m of
  a :< as -> (<| as) <$> f a
  EmptyL  -> pure m
{-# INLINE traverseHead #-}

-- | Traverse the tail of a 'Seq'
traverseTail :: Simple Traversal (Seq a) a
traverseTail f m = case viewl m of
  a :< as -> (a <|) <$> traverse f as
  EmptyL  -> pure m
{-# INLINE traverseTail #-}

-- | Traverse the last element of a 'Seq'
traverseLast :: Simple Traversal (Seq a) a
traverseLast f m = case viewr m of
  as :> a -> (as |>) <$> f a
  EmptyR  -> pure m
{-# INLINE traverseLast #-}

-- | Traverse all but the last element of a 'Seq'
traverseInit :: Simple Traversal (Seq a) a
traverseInit f m = case viewr m of
  as :> a -> (|> a) <$> traverse f as
  EmptyR  -> pure m
{-# INLINE traverseInit #-}

-- | Traverse the first @n@ elements of a 'Seq'
traverseTo :: Int -> Simple Traversal (Seq a) a
traverseTo n f m = case Seq.splitAt n m of
  (l,r) -> (>< r) <$> traverse f l
{-# INLINE traverseTo #-}

-- | Traverse all but the first @n@ elements of a 'Seq'
traverseFrom :: Int -> Simple Traversal (Seq a) a
traverseFrom n f m = case Seq.splitAt n m of
  (l,r) -> (l ><) <$> traverse f r
{-# INLINE traverseFrom #-}

-- | Travere all the elements numbered from @i@ to @j@ of a 'Seq'
traverseSlice :: Int -> Int -> Simple Traversal (Seq a) a
traverseSlice i j f s = case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> (\n -> l >< n >< r) <$> traverse f m
{-# INLINE traverseSlice #-}
