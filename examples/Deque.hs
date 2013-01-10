{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Deque where

import Control.Applicative
import Control.Lens
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Default
import Data.Foldable
import Data.Function
import Data.Traversable
import Data.Monoid

-- | A Banker's deque based on Chris Okasaki's \"Purely Functional Data Structures\"
data Deque a = BD !Int [a] !Int [a]
  deriving Show

fromList :: [a] -> Deque a
fromList = Prelude.foldr cons def

instance Eq a => Eq (Deque a) where
  (==) = (==) `on` toList

instance Ord a => Ord (Deque a) where
  compare = compare `on` toList

instance Functor Deque where
  fmap = fmapDefault

instance Foldable Deque where
  foldMap = foldMapDefault

instance Traversable Deque where
  traverse h (BD lf f lr r) = (BD lf ?? lr) <$> traverse h f <*> backwards traverse h r

instance Default (Deque a) where
  def = BD 0 [] 0 []

check :: Int -> [a] -> Int -> [a] -> Deque a
check lf f lr r
  | lf > 3*lr + 1, i <- div (lf + lr) 2, (f',f'') <- splitAt i f = BD i f' (lf + lr - i) (r ++ reverse f'')
  | lr > 3*lf + 1, j <- div (lf + lr) 2, (r',r'') <- splitAt j r = BD (lf + lr - j) (f ++ reverse r'') j r'
  | otherwise = BD lf f lr r

instance Cons (Deque a) (Deque b) a b where
  _Cons = prism (\(x,BD lf f lr r) -> check (lf + 1) (x : f) lr r) $ \ (BD lf f lr r) ->
    if lf + lr == 0
    then Left def
    else Right $ case f of
      []     -> (head r, def)
      (x:xs) -> (x, check (lf - 1) xs lr r)

instance Snoc (Deque a) (Deque b) a b where
  _Snoc = prism (\(BD lf f lr r,x) -> check lf f (lr + 1) (x : r)) $ \ (BD lf f lr r) ->
    if lf + lr == 0
    then Left def
    else Right $ case r of
      []     -> (def, head f)
      (x:xs) -> (check lf f (lr - 1) xs, x)
