{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main (templates)
-- Copyright   :  (C) 2012-14 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This test suite validates that we are able to generate usable lenses with
-- template haskell.
--
-- The commented code summarizes what will be auto-generated below
-----------------------------------------------------------------------------
module Main where

import Control.Lens
import Data.List (nub)
-- import Test.QuickCheck (quickCheck)

data Bar a b c = Bar { _baz :: (a, b) }
makeLenses ''Bar
-- baz :: Lens (Bar a b c) (Bar a' b' c) (a,b) (a',b')
-- upgrades to:
-- baz :: Iso (Bar a b c) (Bar a' b' c') (a, b) (a', b')

data Quux a b = Quux { _quaffle :: Int, _quartz :: Double }
makeLenses ''Quux
-- quaffle :: Lens (Quux a b) (Quux a' b') Int Int
-- quartz :: Lens (Quux a b) (Quux a' b') Double Double

data Quark a = Qualified   { _gaffer :: a }
             | Unqualified { _gaffer :: a, _tape :: a }
makeLenses ''Quark
-- gaffer :: Simple Lens (Quark a) a
-- tape :: Simple Traversal (Quark a) a

data Hadron a b = Science { _a1 :: a, _a2 :: a, _c :: b }
makeLenses ''Hadron
-- a1 :: Simple Lens (Hadron a b) a
-- a2 :: Simple Lens (Hadron a b) a
-- c :: Lens (Hadron a b) (Hadron a b') b b'

data Perambulation a b
  = Mountains { _terrain :: a, _altitude :: b }
  | Beaches   { _terrain :: a, _dunes :: a }
makeLenses ''Perambulation
-- terrain :: Simple Lens (Perambulation a b) a
-- altitude :: Traversal (Perambulation a b) (Parambulation a b') b b'
-- dunes :: Simple Traversal (Perambulation a b) a
makeLensesFor [("_terrain", "allTerrain"), ("_dunes", "allTerrain")] ''Perambulation
-- allTerrain :: Traversal (Perambulation a b) (Perambulation a' b) a a'

data LensCrafted a = Still { _still :: a }
                   | Works { _still :: a }
makeLenses ''LensCrafted
-- still :: Lens (LensCrafted a) (LensCrafted b) a b

data Danger a = Zone { _highway :: a }
              | Twilight
--makeLensesWith (partialLenses .~ True $ buildTraversals .~ False $ lensRules) ''Danger
-- highway :: Lens (Danger a) (Danger a') a a'

data Task a = Task
  { taskOutput :: a -> IO ()
  , taskState :: a
  , taskStop :: IO ()
  }

makeLensesFor [("taskOutput", "outputLens"), ("taskState", "stateLens"), ("taskStop", "stopLens")] ''Task

data Mono a = Mono { _monoFoo :: a, _monoBar :: Int }
makeClassy ''Mono
-- class HasMono t where
--   mono :: Simple Lens t Mono
-- instance HasMono Mono where
--   mono = id
-- monoFoo :: HasMono t => Simple Lens t Int
-- monoBar :: HasMono t => Simple Lens t Int

data Nucleosis = Nucleosis { _nuclear :: Mono Int }
makeClassy ''Nucleosis
-- class HasNucleosis t where
--   nucleosis :: Simple Lens t Nucleosis
-- instance HasNucleosis Nucleosis
-- nuclear :: HasNucleosis t => Simple Lens t Mono

instance HasMono Nucleosis Int where
  mono = nuclear

-- Dodek's example
data Foo = Foo { _fooX, _fooY :: Int }
makeClassy ''Foo


data Dude a = Dude
    { dudeLevel        :: Int
    , dudeAlias        :: String
    , dudeLife         :: ()
    , dudeThing        :: a
    }
data Lebowski a = Lebowski
    { _lebowskiAlias    :: String
    , _lebowskiLife     :: Int
    , _lebowskiMansion  :: String
    , _lebowskiThing    :: Maybe a
    }
data AbideConfiguration a = AbideConfiguration
    { _acLocation       :: String
    , _acDuration       :: Int
    , _acThing          :: a
    }

makeFields ''Dude
makeFields ''Lebowski
makeFields ''AbideConfiguration

dudeDrink :: String
dudeDrink      = (Dude 9 "El Duderino" () "white russian")      ^. thing 
lebowskiCarpet :: Maybe String
lebowskiCarpet = (Lebowski "Mr. Lebowski" 0 "" (Just "carpet")) ^. thing
abideAnnoyance :: String
abideAnnoyance = (AbideConfiguration "the tree" 10 "the wind")  ^. thing

declareLenses [d|
  data Quark1 a = Qualified1   { gaffer1 :: a }
                | Unqualified1 { gaffer1 :: a, tape1 :: a }
  |]
-- data Quark1 a = Qualified1 a | Unqualified1 a a
-- gaffer1 :: Lens' (Quark1 a) a
-- tape1 :: Traversal (Quark1 a) (Quark1 b) a b

declarePrisms [d|
  data Exp = Lit Int | Var String | Lambda { bound::String, body::Exp }
  |]
-- data Exp = Lit Int | Var String | Lambda { bound::String, body::Exp }
-- _Lit :: Prism' Exp Int
-- _Var :: Prism' Exp String
-- _Lambda :: Prism' Exp (String, Exp)

declarePrisms [d|
  data Banana = Banana Int String
  |]
-- data Banana = Banana Int String
-- _Banana :: Iso' Banana (Int, String)
cavendish :: Banana
cavendish = _Banana # (4, "Cavendish")

banana :: Iso' (Int, String) Banana
banana = from _Banana

data family Family a b c

#if __GLASGOW_HASKELL >= 706
declareLenses [d|
  data instance Family Int (a, b) a = FamilyInt { fm0 :: (b, a), fm1 :: Int }
  |]
-- data instance Family Int (a, b) a = FamilyInt a b
-- fm0 :: Lens (Family Int (a, b) a) (Family Int (a', b') a') (b, a) (b', a')
-- fm1 :: Lens' (Family Int (a, b) a) Int
#endif

class Class a where
  data Associated a
  method :: a -> Int

declareLenses [d|
  instance Class Int where
    data Associated Int = AssociatedInt { mochi :: Double }
    method = id
  |]

-- instance Class Int where
--   data Associated Int = AssociatedInt Double
--   method = id
-- mochi :: Iso' (Associated Int) Double

#if __GLASGOW_HASKELL >= 706
declareFields [d|
  data DeclaredFields f a
    = DeclaredField1 { declaredFieldsA :: f a    , declaredFieldsB :: Int }
    | DeclaredField2 { declaredFieldsC :: String , declaredFieldsB :: Int }
    deriving (Show)
  |]

declaredFieldsUse1 :: [Bool]
declaredFieldsUse1 = view fieldsA (DeclaredField1 [True] 0)

declaredFieldsUse2 :: [DeclaredFields [] ()]
declaredFieldsUse2 = over (traverse.fieldsB) (+1) [DeclaredField1 [()] 0, DeclaredField2 "" 1]
#endif

data Rank2Tests
  = C1 { _r2length :: forall a. [a] -> Int
       , _r2nub    :: forall a. Eq a=> [a] -> [a]
       }
  | C2 { _r2length :: forall a. [a] -> Int }

makeLenses ''Rank2Tests

useFold1 :: Maybe [Bool]
useFold1  = C1 length nub ^? r2nub . to ($ [False,True,True])
useFold2 :: Maybe [Bool]
useFold2  = C2 length     ^? r2nub . to ($ [False,True,True])
useLength :: Int
useLength = C1 length nub ^. r2length . to ($ [False,True,True])

data PureNoFields = PureNoFieldsA | PureNoFieldsB { _pureNoFields :: Int }
makeLenses ''PureNoFields

main :: IO ()
main = putStrLn "test/templates.hs: ok"
