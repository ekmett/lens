{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
-- | The commented code summarizes what will be auto-generated below
module Main where

import Control.Lens
-- import Test.QuickCheck (quickCheck)

-- newtype Foo a = Foo a
-- makeIso ''Foo
-- foo :: Iso a b (Foo a) (Foo b)

data Bar a b c = Bar { _baz :: (a, b) }
makeLenses ''Bar
-- baz :: Lens (Bar a b c) (Bar a' b' c) (a,b) (a',b')

data Quux a b = Quux { _quaffle :: Int, _quartz :: Double }
makeLenses ''Quux
-- quaffle :: Lens (Quux a b) (Quux a' b') Int Int
-- quartz :: Lens (Quux a b) (Quux a' b') Double Double

data Quark a = Qualified  { _gaffer :: a }
             | Unqualified { _gaffer :: a, tape :: a }
makeLenses ''Quark
-- gaffer :: Simple Lens (Quark a) a

data LensCrafted a = Still { _still :: a }
                   | Works { _still :: a }
makeLenses ''LensCrafted
-- still :: Lens (LensCrafted a) (LensCrafted b) a b

data Mono = Mono { _monoFoo :: Int, _monoBar :: Int }
makeClassy ''Mono
-- class HasMono t where
--   mono :: Simple Lens t Mono
-- instance HasMono Mono where
--   mono = id
-- monoFoo :: HasMono t => Simple Lens t Int
-- monoBar :: HasMono t => Simple Lens t Int

data Nucleosis = Nucleosis { _nuclear :: Mono }
makeClassy ''Nucleosis
-- class HasNucleosis t where
--   nucleosis :: Simple Lens t Nucleosis
-- instance HasNucleosis Nucleosis
-- nuclear :: HasNucleosis t => Simple Lens t Mono

instance HasMono Nucleosis where
  mono = nuclear

main :: IO ()
main = putStrLn "test/templates.hs: ok"
