{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Main (main) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Prelude ()
import Prelude.Compat

import           Control.Lens
import           Control.DeepSeq
import           Criterion.Main
import           Data.Data
import           Data.Data.Lens as Data
#ifdef BENCHMARK_UNIPLATE
import qualified Data.Generics.Uniplate.Direct as Uni
import           Data.Generics.Uniplate.Direct ((|*))
import qualified Data.Generics.Uniplate.DataOnly as UniDataOnly
#endif
import           Generics.Deriving hiding (universe)

data Expr  =  Val !Int
           |  Var String
           |  Neg !Expr
           |  Add !Expr !Expr
           |  Sub !Expr !Expr
           |  Mul !Expr !Expr
           |  Div !Expr !Expr
           deriving (Eq,Show,Data,Generic)

instance NFData Expr where
  rnf (Neg a)   = rnf a
  rnf (Add a b) = rnf a `seq` rnf b
  rnf (Sub a b) = rnf a `seq` rnf b
  rnf (Mul a b) = rnf a `seq` rnf b
  rnf (Div a b) = rnf a `seq` rnf b
  rnf (Val i)   = rnf i
  rnf (Var s)   = rnf s

instance Plated Expr where
  plate f (Neg a)   = Neg <$> f a
  plate f (Add a b) = Add <$> f a <*> f b
  plate f (Sub a b) = Sub <$> f a <*> f b
  plate f (Mul a b) = Mul <$> f a <*> f b
  plate f (Div a b) = Div <$> f a <*> f b
  plate _ t = pure t
  {-# INLINE plate #-}

#ifdef BENCHMARK_UNIPLATE
instance Uni.Uniplate Expr where
  uniplate (Neg a)   = Uni.plate Neg |* a
  uniplate (Add a b) = Uni.plate Add |* a |* b
  uniplate (Sub a b) = Uni.plate Sub |* a |* b
  uniplate (Mul a b) = Uni.plate Mul |* a |* b
  uniplate (Div a b) = Uni.plate Div |* a |* b
  uniplate (Val i)   = Uni.plate (Val i)
  uniplate (Var s)   = Uni.plate (Var s)
  {-# INLINE uniplate #-}
#endif

main :: IO ()
main = defaultMain
  [ bench "universe"                           $ nf (map universe) testsExpr
  , bench "universeOf plate"                   $ nf (map (universeOf plate)) testsExpr
  , bench "universeOf Data.tinplate"           $ nf (map (universeOf Data.tinplate)) testsExpr
  , bench "universeOf Data.template"           $ nf (map (universeOf Data.template)) testsExpr
  , bench "universeOf Data.uniplate"           $ nf (map (universeOf Data.uniplate)) testsExpr
  , bench "universeOf (cloneTraversal plate)"  $ nf (map (universeOf (cloneTraversal plate))) testsExpr
#ifdef BENCHMARK_UNIPLATE
  , bench "Direct.universe"                    $ nf (map Uni.universe) testsExpr
  , bench "DataOnly.universe"                  $ nf (map UniDataOnly.universe) testsExpr
#endif
  ]

testsExpr :: [Expr]
testsExpr = [Val 3,Val 2,Val 6,Neg (Neg (Var "dus")),Mul (Div (Add (Val 4) (Var "kxm")) (Sub (Mul (Div (Var "") (Var "")) (Var "w")) (Var "ed"))) (Var "whpd"),Val 6,Val 4,Val 2,Var "a",Val 1,Div (Var "") (Val 0),Var "",Var "",Val (-3),Val 3,Sub (Var "") (Val 2),Neg (Var "dlp"),Div (Val 0) (Var "sd"),Val (-2),Val (-3),Var "g",Mul (Val 3) (Var "i"),Val 1,Var "ul",Div (Add (Var "") (Var "")) (Mul (Div (Val 0) (Neg (Val 0))) (Neg (Neg (Mul (Var "") (Val 0))))),Var "z",Sub (Neg (Add (Var "") (Val 0))) (Var ""),Neg (Sub (Mul (Val 0) (Val 2)) (Val 5)),Val 0,Val 0,Mul (Val (-4)) (Sub (Val 5) (Neg (Div (Div (Val 0) (Sub (Neg (Sub (Val (-3)) (Mul (Mul (Var "ap") (Val 3)) (Add (Add (Add (Var "owre") (Add (Add (Var "avj") (Val 3)) (Var "vhi"))) (Mul (Val 2) (Var "hak"))) (Val 2))))) (Var "nf"))) (Add (Sub (Val 5) (Sub (Var "pkjyh") (Val 2))) (Var "lsiu"))))),Var "u",Val 1,Neg (Add (Add (Var "") (Val 1)) (Sub (Add (Add (Val (-3)) (Mul (Val 1) (Var "pfe"))) (Var "yv")) (Mul (Var "") (Var "jfq")))),Val 2,Div (Div (Div (Div (Var "xrgykq") (Mul (Var "kyfu") (Val 2))) (Sub (Var "v") (Val 0))) (Sub (Val 6) (Val 2))) (Val 3),Var "",Var "",Add (Var "ob") (Sub (Mul (Neg (Val 2)) (Val 6)) (Add (Mul (Val 6) (Sub (Add (Var "wue") (Mul (Var "hgsuj") (Neg (Div (Var "hr") (Var "ozvsb"))))) (Sub (Var "j") (Div (Var "yeyhvq") (Val (-6)))))) (Var "b"))),Div (Add (Div (Div (Neg (Val 4)) (Var "")) (Var "yfx")) (Div (Sub (Var "") (Sub (Var "np") (Mul (Val 3) (Var "mxr")))) (Mul (Var "m") (Var "kkhbf")))) (Neg (Sub (Var "yie") (Val 1))),Neg (Var ""),Var "liuh",Var "pbqg",Var "",Neg (Div (Sub (Add (Val (-1)) (Var "onynvr")) (Neg (Var "tqjsay"))) (Add (Val 4) (Var "yorkb"))),Val 1,Add (Mul (Neg (Div (Val (-1)) (Var "u"))) (Sub (Var "") (Neg (Val 1)))) (Var "h"),Var "",Add (Mul (Sub (Var "em") (Val 0)) (Add (Val (-2)) (Val 1))) (Var ""),Add (Mul (Add (Div (Add (Val 0) (Mul (Mul (Var "e") (Add (Val 1) (Var ""))) (Neg (Neg (Div (Add (Div (Neg (Val 1)) (Div (Val (-1)) (Mul (Add (Div (Val (-1)) (Mul (Mul (Val 1) (Val 1)) (Mul (Var "t") (Val (-1))))) (Val 1)) (Val 1)))) (Add (Neg (Add (Val 0) (Var "k"))) (Mul (Neg (Div (Sub (Sub (Var "u") (Val 1)) (Val 1)) (Sub (Neg (Var "")) (Sub (Var "b") (Val (-1)))))) (Neg (Var ""))))) (Val 0)))))) (Val 0)) (Var "a")) (Var "")) (Val (-1)),Var "xijsnp",Div (Var "h") (Neg (Val 5)),Div (Var "dmzlh") (Add (Val 6) (Val (-2))),Neg (Add (Val 0) (Var "")),Add (Add (Add (Sub (Val 4) (Var "nfse")) (Var "o")) (Add (Val 2) (Div (Var "mtqdx") (Val (-3))))) (Val 3),Neg (Var "c"),Var "sr",Mul (Add (Sub (Neg (Val 1)) (Sub (Div (Add (Sub (Add (Sub (Sub (Var "gd") (Mul (Var "v") (Var "d"))) (Var "")) (Val 1)) (Add (Val 2) (Var ""))) (Var "kk")) (Div (Var "fw") (Add (Val 1) (Var "f")))) (Var ""))) (Val 2)) (Add (Neg (Div (Var "") (Val 0))) (Add (Var "") (Add (Var "s") (Add (Mul (Var "") (Val (-1))) (Val 1))))),Val 1,Var "",Sub (Var "vbnzahx") (Val (-5)),Var "nl",Val 0,Add (Mul (Neg (Mul (Var "") (Var "mvil"))) (Var "")) (Neg (Var "zxl")),Val (-3),Var "",Var "e",Add (Div (Sub (Val 0) (Add (Val 5) (Val 7))) (Mul (Var "") (Var "qz"))) (Val 4),Add (Val (-1)) (Neg (Var "lk")),Add (Add (Var "u") (Mul (Val 1) (Var "h"))) (Sub (Mul (Div (Val 1) (Div (Var "t") (Neg (Var "")))) (Var "")) (Mul (Val 1) (Neg (Div (Neg (Var "")) (Var ""))))),Val 0,Val 0,Val (-7),Mul (Var "") (Val 0),Mul (Add (Val (-6)) (Add (Val 2) (Sub (Div (Var "z") (Var "gbb")) (Var "vddnpsl")))) (Add (Add (Add (Var "") (Sub (Div (Val 3) (Neg (Div (Add (Var "cfvgz") (Add (Sub (Var "htd") (Sub (Var "mhbl") (Var "un"))) (Val 3))) (Val (-3))))) (Var ""))) (Val 5)) (Neg (Mul (Val 0) (Var "sufvvj")))),Sub (Div (Neg (Add (Add (Neg (Add (Var "") (Val 0))) (Var "")) (Sub (Val 0) (Val 0)))) (Val 0)) (Add (Neg (Div (Div (Add (Sub (Add (Add (Neg (Var "")) (Val 0)) (Val 0)) (Add (Neg (Add (Neg (Var "")) (Neg (Val 0)))) (Val 0))) (Div (Val 0) (Val 0))) (Val 0)) (Val 0))) (Var "")),Var "",Sub (Div (Val 0) (Div (Add (Val 1) (Neg (Div (Neg (Var "y")) (Val 0)))) (Var ""))) (Sub (Div (Var "t") (Var "")) (Neg (Var "s"))),Mul (Div (Sub (Var "") (Var "")) (Add (Val 0) (Sub (Div (Var "yr") (Neg (Var "o"))) (Val 1)))) (Var "u"),Var "odmn",Div (Var "uddqy") (Val 3),Var "",Sub (Val 2) (Neg (Val (-1))),Div (Mul (Var "sox") (Val (-3))) (Val (-3)),Var "qv",Var "xmbnts",Var "j",Mul (Val 6) (Mul (Var "fryndq") (Neg (Val 6))),Var "",Var "",Val (-1),Val 7,Add (Var "dg") (Val 1),Neg (Val 1),Val 0,Var "xnm",Sub (Div (Div (Var "miwi") (Var "mbh")) (Val 3)) (Val 3),Neg (Val (-4)),Var "ndubxoa",Var ""]
