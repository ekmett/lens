{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import Control.Lens ((&), (^.))
import Control.Lens.TH (makeLenses)
import Control.Lens.Build (record, field, build)
import Control.Lens.Build.TH (makeBuild)
import System.Exit (exitFailure)

-- Case 1: makeBuild stands on its own — no makeLenses for this type.
data Bare = Bare { _p :: Int, _q :: Int } deriving (Eq, Show)
makeBuild ''Bare

bare :: Bare
bare = record & field @"p" 1 & field @"q" 2 & build

-- Case 2: with makeLenses too, the @"a" / @"b" names line up with the lenses,
-- so we can read the built record back through them.
data Foo = Foo { _a :: Int, _b :: Bool } deriving (Eq, Show)
makeLenses ''Foo   -- generates lenses  a :: Lens' Foo Int,  b :: Lens' Foo Bool
makeBuild  ''Foo   -- generates the RecordBuilder / SetField instances

foo :: Foo
foo = record
    & field @"a" 1
    & field @"b" True
    & build

main :: IO ()
main
  | bare == Bare 1 2            -- makeBuild works with no makeLenses in sight
  , foo == Foo 1 True           -- the built record is what we asked for
  , foo ^. a == 1, foo ^. b     -- and @"a" / @"b" match the makeLenses lenses
  = putStrLn "lens-build: ok"
  | otherwise = do
      putStrLn ("lens-build: FAILED, got " ++ show bare ++ " / " ++ show foo)
      exitFailure
