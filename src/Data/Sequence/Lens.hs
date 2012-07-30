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

at :: Int -> Simple Lens (Seq a) a
at i f m = (\a -> update i a m) <$> f (index m i)

-- * Sequence isomorphisms

viewL :: Simple Lens (Seq a) (ViewL a)
viewL f m = go <$> f (viewl m) where
  go EmptyL = mempty
  go (a :< as) = a <| as
{-# INLINE viewL #-}

viewR :: Simple Lens (Seq a) (ViewR a)
viewR f m = go <$> f (viewr m) where
  go EmptyR = mempty
  go (as :> a) = as |> a
{-# INLINE viewR #-}

-- * Traversals

traverseHead :: Simple Traversal (Seq a) a
traverseHead f m = case viewl m of
  a :< as -> (<| as) <$> f a
  EmptyL  -> pure m
{-# INLINE traverseHead #-}

traverseTail :: Simple Traversal (Seq a) a
traverseTail f m = case viewl m of
  a :< as -> (a <|) <$> traverse f as
  EmptyL  -> pure m
{-# INLINE traverseTail #-}

traverseLast :: Simple Traversal (Seq a) a
traverseLast f m = case viewr m of
  as :> a -> (as |>) <$> f a
  EmptyR  -> pure m
{-# INLINE traverseLast #-}

traverseInit :: Simple Traversal (Seq a) a
traverseInit f m = case viewr m of
  as :> a -> (|> a) <$> traverse f as
  EmptyR  -> pure m
{-# INLINE traverseInit #-}

traverseTo :: Int -> Simple Traversal (Seq a) a
traverseTo n f m = case Seq.splitAt n m of
  (l,r) -> (>< r) <$> traverse f l
{-# INLINE traverseTo #-}

traverseFrom :: Int -> Simple Traversal (Seq a) a
traverseFrom n f m = case Seq.splitAt n m of
  (l,r) -> (l ><) <$> traverse f r
{-# INLINE traverseFrom #-}

traverseSlice :: Int -> Int -> Simple Traversal (Seq a) a
traverseSlice i j f s = case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> (\n -> l >< n >< r) <$> traverse f m
{-# INLINE traverseSlice #-}
