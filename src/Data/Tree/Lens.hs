{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE Rank2Types #-}
module Data.Tree.Lens
  ( root
  , children
  ) where

import Control.Lens
import Data.Functor
import Data.Tree

root :: Simple Lens (Tree a) a
root f (Node a as) = (`Node` as) <$> f a

children :: Simple Traversal (Tree a) (Tree a)
children f (Node a as) = Node a <$> traverse f as
