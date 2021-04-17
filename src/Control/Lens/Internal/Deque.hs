{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#include "lens-common.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Deque
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module is designed to be imported qualified.
-----------------------------------------------------------------------------
module Control.Lens.Internal.Deque
  ( Deque(..)
  , size
  , fromList
  , null
  , singleton
  ) where

import Prelude ()

import Control.Lens.Cons
import Control.Lens.Fold
import Control.Lens.Indexed hiding ((<.>))
import Control.Lens.Internal.Prelude hiding (null)
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Monad
import Data.Foldable (toList)
import Data.Function
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Functor.Reverse

-- $setup
-- >>> import Control.Applicative (empty)

-- | A Banker's deque based on Chris Okasaki's \"Purely Functional Data Structures\"
data Deque a = BD !Int [a] !Int [a]
  deriving Show

-- | /O(1)/. Determine if a 'Deque' is 'empty'.
--
-- >>> Control.Lens.Internal.Deque.null empty
-- True
--
-- >>> Control.Lens.Internal.Deque.null (singleton 1)
-- False
null :: Deque a -> Bool
null (BD lf _ lr _) = lf + lr == 0
{-# INLINE null #-}

-- | /O(1)/. Generate a singleton 'Deque'
--
-- >>> singleton 1
-- BD 1 [1] 0 []
singleton :: a -> Deque a
singleton a = BD 1 [a] 0 []
{-# INLINE singleton #-}

-- | /O(1)/. Calculate the size of a 'Deque'
--
-- >>> size (fromList [1,4,6])
-- 3
size :: Deque a -> Int
size (BD lf _ lr _) = lf + lr
{-# INLINE size #-}

-- | /O(n)/ amortized. Construct a 'Deque' from a list of values.
--
-- >>> fromList [1,2]
-- BD 1 [1] 1 [2]
fromList :: [a] -> Deque a
fromList = foldr cons empty
{-# INLINE fromList #-}

instance Eq a => Eq (Deque a) where
  (==) = (==) `on` toList
  {-# INLINE (==) #-}

instance Ord a => Ord (Deque a) where
  compare = compare `on` toList
  {-# INLINE compare #-}

instance Functor Deque where
  fmap h (BD lf f lr r) = BD lf (fmap h f) lr (fmap h r)
  {-# INLINE fmap #-}

instance FunctorWithIndex Int Deque where
  imap h (BD lf f lr r) = BD lf (imap h f) lr (imap (\j -> h (n - j)) r)
    where !n = lf + lr

instance Apply Deque where
  fs <.> as = fromList (toList fs <.> toList as)
  {-# INLINE (<.>) #-}

instance Applicative Deque where
  pure a = BD 1 [a] 0 []
  {-# INLINE pure #-}
  fs <*> as = fromList (toList fs <*> toList as)
  {-# INLINE (<*>) #-}

instance Alt Deque where
  xs <!> ys
    | size xs < size ys = foldr cons ys xs
    | otherwise         = foldl snoc xs ys
  {-# INLINE (<!>) #-}

instance Plus Deque where
  zero = BD 0 [] 0 []
  {-# INLINE zero #-}

instance Alternative Deque where
  empty = BD 0 [] 0 []
  {-# INLINE empty #-}
  xs <|> ys
    | size xs < size ys = foldr cons ys xs
    | otherwise         = foldl snoc xs ys
  {-# INLINE (<|>) #-}

instance Reversing (Deque a) where
  reversing (BD lf f lr r) = BD lr r lf f
  {-# INLINE reversing #-}

instance Bind Deque where
  ma >>- k = fromList (toList ma >>= toList . k)
  {-# INLINE (>>-) #-}

instance Monad Deque where
  return = pure
  {-# INLINE return #-}
  ma >>= k = fromList (toList ma >>= toList . k)
  {-# INLINE (>>=) #-}

instance MonadPlus Deque where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance Foldable Deque where
  foldMap h (BD _ f _ r) = foldMap h f `mappend` getDual (foldMap (Dual #. h) r)
  {-# INLINE foldMap #-}

instance FoldableWithIndex Int Deque where
  ifoldMap h (BD lf f lr r) = ifoldMap h f `mappend` getDual (ifoldMap (\j -> Dual #. h (n - j)) r)
    where !n = lf + lr
  {-# INLINE ifoldMap #-}

instance Traversable Deque where
  traverse h (BD lf f lr r) = (BD lf ?? lr) <$> traverse h f <*> backwards traverse h r
  {-# INLINE traverse #-}

instance TraversableWithIndex Int Deque where
  itraverse h (BD lf f lr r) = (\f' r' -> BD lr f' lr (getReverse r')) <$> itraverse h f <*> itraverse (\j -> h (n - j)) (Reverse r)
    where !n = lf + lr
  {-# INLINE itraverse #-}

instance Semigroup (Deque a) where
  xs <> ys
    | size xs < size ys = foldr cons ys xs
    | otherwise         = foldl snoc xs ys
  {-# INLINE (<>) #-}

instance Monoid (Deque a) where
  mempty = BD 0 [] 0 []
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend xs ys
    | size xs < size ys = foldr cons ys xs
    | otherwise         = foldl snoc xs ys
  {-# INLINE mappend #-}
#endif

-- | Check that a 'Deque' satisfies the balance invariants and rebalance if not.
check :: Int -> [a] -> Int -> [a] -> Deque a
check lf f lr r
  | lf > 3*lr + 1, i <- div (lf + lr) 2, (f',f'') <- splitAt i f = BD i f' (lf + lr - i) (r ++ reverse f'')
  | lr > 3*lf + 1, j <- div (lf + lr) 2, (r',r'') <- splitAt j r = BD (lf + lr - j) (f ++ reverse r'') j r'
  | otherwise = BD lf f lr r
{-# INLINE check #-}

instance Cons (Deque a) (Deque b) a b where
  _Cons = prism (\(x,BD lf f lr r) -> check (lf + 1) (x : f) lr r) $ \ (BD lf f lr r) ->
    if lf + lr == 0
    then Left empty
    else Right $ case f of
      []     -> (head r, empty)
      (x:xs) -> (x, check (lf - 1) xs lr r)
  {-# INLINE _Cons #-}

instance Snoc (Deque a) (Deque b) a b where
  _Snoc = prism (\(BD lf f lr r,x) -> check lf f (lr + 1) (x : r)) $ \ (BD lf f lr r) ->
    if lf + lr == 0
    then Left empty
    else Right $ case r of
      []     -> (empty, head f)
      (x:xs) -> (check lf f (lr - 1) xs, x)
  {-# INLINE _Snoc #-}
