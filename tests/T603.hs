{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 904
{-# OPTIONS_GHC -Werror=gadt-mono-local-binds #-}
#endif
-- | 'makeHListPrisms' and 'makeHListClassyPrisms'. This module enables neither
-- @GADTs@ nor @TypeFamilies@, because the generated code must not need them.
module T603 where

import Control.Lens
import Control.Lens.HList
import Control.Monad (unless)
import System.Exit (die)

data FooHL a = FooI Int | BarA a | BazIC Int Char
  deriving (Show, Eq)
makeHListPrisms ''FooHL

check_FooI :: Prism' (FooHL a) (HList '[Int])
check_FooI = _FooI

-- A field that mentions a type variable gives a type-changing prism.
check_BarA :: Prism (FooHL a) (FooHL b) (HList '[a]) (HList '[b])
check_BarA = _BarA

check_BazIC :: Prism' (FooHL a) (HList '[Int, Char])
check_BazIC = _BazIC

-- Constructors with 0 or 1 fields are wrapped like the others.
data UnitHL = NoneU | OneU Int
  deriving (Show, Eq)
makeHListPrisms ''UnitHL

check_NoneU :: Prism' UnitHL (HList '[])
check_NoneU = _NoneU

check_OneU :: Prism' UnitHL (HList '[Int])
check_OneU = _OneU

-- A type with one constructor gets an Iso.
data PairHL = PairHL Int String
  deriving (Show, Eq)
makeHListPrisms ''PairHL

check_PairHL :: Iso' PairHL (HList '[Int, String])
check_PairHL = _PairHL

-- Classy prisms.
data Shape = Circle Int | Rect Int Int | Pt
  deriving (Show, Eq)
makeHListClassyPrisms ''Shape

check_Rect :: AsShape r => Prism' r (HList '[Int, Int])
check_Rect = _Rect

check_Pt :: AsShape r => Prism' r (HList '[])
check_Pt = _Pt

-- An existential constructor gets a Review.
data Box = forall a. Show a => Box a Int | PlainBox Int
makeHListPrisms ''Box

check_Box :: Show a => Review Box (HList '[a, Int])
check_Box = _Box

checks :: [Bool]
checks =
  [ _PairHL # (4 :# "Cavendish" :# HNil) == PairHL 4 "Cavendish"
  , PairHL 4 "Cavendish" ^. _PairHL == 4 :# "Cavendish" :# HNil
  , (BazIC 1 'x' ^? _BazIC) == Just (1 :# 'x' :# HNil)
  , (FooI 1 ^? _BazIC) == Nothing
  , (BarA 'c' ^? _BarA) == Just ('c' :# HNil)
  , _OneU # (3 :# HNil) == OneU 3
  , (NoneU ^? _NoneU) == Just HNil
  , (OneU 3 ^? _NoneU) == Nothing
  , _Rect # (2 :# 3 :# HNil) == Rect 2 3
  , (Rect 2 3 ^? _Rect) == Just (2 :# 3 :# HNil)
  , (Circle 1 ^? _Rect) == Nothing
  , (Pt ^? _Pt) == Just HNil
  , case _Box # (True :# 7 :# HNil) of
      Box a n  -> show a == "True" && n == 7
      PlainBox _ -> False
  ]

runChecks :: IO ()
runChecks = unless (and checks) (die "T603: an HList prism check failed")
