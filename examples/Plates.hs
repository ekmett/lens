{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative
import Control.Lens
import Data.Plated

data Expr = Var Int | Pos Expr String | Neg Expr | Add Expr Expr
data Stmt = Seq [Stmt] | Sel [Expr] | Let String Expr

instance Plated Expr where
  plates f (Var x  ) = pure (Var x)
  plates f (Pos x y) = Pos <$> f x <*> pure y
  plates f (Neg x  ) = Neg <$> f x
  plates f (Add x y) = Add <$> f x <*> f y

instance Biplated Expr Expr

instance Plated Stmt where
  plates f (Seq xs) = Seq <$> traverse f xs
  plates f (Sel xs) = pure (Sel xs)
  plates f (Let x y) = pure (Let x y)

instance Biplated Stmt Stmt
instance Biplated Stmt Expr where
  biplates f (Seq xs)  = Seq <$> traverse (biplates f) xs
  biplates f (Sel xs)  = Sel <$> traverse f xs
  biplates f (Let x y) = Let x <$> f y
