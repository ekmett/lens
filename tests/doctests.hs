module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"
  , "-idist/build/autogen"
  , "-optP-include", "-optPdist/build/autogen/cabal_macros.h"
  , "src/Control/Lens/Action.hs"
  , "src/Control/Lens/Fold.hs"
  , "src/Control/Lens/Getter.hs"
  , "src/Control/Lens/Setter.hs"
  , "src/Data/Array/Lens.hs"
  , "src/Data/Bits/Lens.hs"
  , "src/Data/IntMap/Lens.hs"
  , "src/Data/IntSet/Lens.hs"
  , "src/Data/List/Lens.hs"
  , "src/Data/Map/Lens.hs"
  , "src/Data/Set/Lens.hs"
  , "src/GHC/Generics/Lens.hs"
  ]
