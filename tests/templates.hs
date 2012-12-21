{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The commented code summarizes what will be auto-generated below
module Main where

import Control.Lens
-- import Test.QuickCheck (quickCheck)

data Bar a b c = Bar { _baz :: (a, b) }
makeLenses ''Bar
-- baz :: Lens (Bar a b c) (Bar a' b' c) (a,b) (a',b')

data Quux a b = Quux { _quaffle :: Int, _quartz :: Double }
makeLenses ''Quux
-- quaffle :: Lens (Quux a b) (Quux a' b') Int Int
-- quartz :: Lens (Quux a b) (Quux a' b') Double Double

data Quark a = Qualified   { _gaffer :: a }
             | Unqualified { _gaffer :: a, _tape :: a }
makeLenses ''Quark
-- gaffer :: Simple Lens (Quark a) a
-- tape :: Simple Traversal (Quark a) a

data Hadron a b = Science { _a1 :: a, _a2 :: a, _b :: b }
makeLenses ''Hadron
-- a1 :: Simple Lens (Hadron a b) a
-- a2 :: Simple Lens (Hadron a b) a
-- b :: Lens (Hadron a b) (Hadron a b') b b'

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
makeLensesWith (partialLenses .~ True $ buildTraversals .~ False $ lensRules) ''Danger
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

main :: IO ()
main = putStrLn "test/templates.hs: ok"
