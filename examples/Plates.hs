{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, DeriveDataTypeable #-}
module Plates where

import Control.Lens
import GHC.Generics
import Data.Data

data Expr = Var Int | Pos Expr String | Neg Expr | Add Expr Expr deriving (Eq,Ord,Show,Read,Generic,Data)
data Stmt = Seq [Stmt] | Sel [Expr] | Let String Expr deriving (Eq,Ord,Show,Read,Generic,Data)

instance Plated Expr where
  plate _ (Var x  ) = pure (Var x)
  plate f (Pos x y) = Pos <$> f x <*> pure y
  plate f (Neg x  ) = Neg <$> f x
  plate f (Add x y) = Add <$> f x <*> f y

instance Plated Stmt where
  plate f (Seq xs)  = Seq <$> traverse f xs
  plate _ (Sel xs)  = pure (Sel xs)
  plate _ (Let x y) = pure (Let x y)

exprs :: Traversal' Stmt Expr
exprs f (Seq xs)  = Seq <$> traverse (exprs f) xs
exprs f (Sel xs)  = Sel <$> traverse f xs
exprs f (Let x y) = Let x <$> f y
