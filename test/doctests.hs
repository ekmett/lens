module Main where

import           Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"
  , "-idist/build/autogen"
  , "-optP-include", "-optPdist/build/autogen/cabal_macros.h"
  , "src/Control/Lens.hs"
  , "src/Data/IntMap/Lens.hs"
  , "src/Data/IntSet/Lens.hs"
  , "src/Data/List/Lens.hs"
  , "src/Data/Map/Lens.hs"
  , "src/Data/Set/Lens.hs"
  ]
