{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, DeriveDataTypeable #-}
import Control.Applicative
import Control.Lens
import Control.Plated
import GHC.Generics
import GHC.Generics.Lens
import Data.Data

data Expr = Var Int | Pos Expr String | Neg Expr | Add Expr Expr deriving (Eq,Ord,Show,Read,Generic,Data,Typeable)
data Stmt = Seq [Stmt] | Sel [Expr] | Let String Expr deriving (Eq,Ord,Show,Read,Generic,Data,Typeable)

instance Plated Expr where
  plates = every
{-
  plates f (Var x  ) = pure (Var x)
  plates f (Pos x y) = Pos <$> f x <*> pure y
  plates f (Neg x  ) = Neg <$> f x
  plates f (Add x y) = Add <$> f x <*> f y
-}

instance Plated Stmt where
  plates = every
{-
  plates f (Seq xs) = Seq <$> traverse f xs
  plates f (Sel xs) = pure (Sel xs)
  plates f (Let x y) = pure (Let x y)
-}

stmtExprs :: Simple Traversal Stmt Expr
stmtExprs = every
{-
stmtExprs f (Seq xs)  = Seq <$> traverse (stmtExprs f) xs
stmtExprs f (Sel xs)  = Sel <$> traverse f xs
stmtExprs f (Let x y) = Let x <$> f y
-}
