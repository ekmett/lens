module Main
       ( main -- :: IO ()
       ) where

import Control.Lens
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "rezip"
      [ bench "rezip"             $ nf tugAndRezip1 ['a'..'z']
      , bench "farthest leftward" $ nf tugAndRezip2 ['a'..'z']
      , bench "leftmost"          $ nf tugAndRezip3 ['a'..'z']
      , bench "tugTo"             $ nf tugAndRezip4 ['a'..'z']
      ]
  , bgroup "zipper creation"
      [ bench "over traverse id"  $ nf (over traverse id) ['a'..'z']
      , bench "zipper"            $ nf zipTraverseRezip   ['a'..'z']
      ]
  , bgroup "downward"
      [ bench "downward _1"       $ nf downwardAndRezip1 (['a'..'z'],['z'..'a'])
      , bench "fromWithin"        $ nf downwardAndRezip2 (['a'..'z'],['z'..'a'])
      ]
  ]

-- What's the fastest rezip of all?
tugAndRezip1, tugAndRezip2, tugAndRezip3 :: String -> String
tugAndRezip1 xs = zipntugs 25 xs & focus .~ 'a' & rezip
tugAndRezip2 xs = zipntugs 25 xs & focus .~ 'b' & farthest leftward & rezip
tugAndRezip3 xs = zipntugs 25 xs & focus .~ 'c' & leftmost & rezip
tugAndRezip4 xs = zipntugs 25 xs & focus .~ 'd' & tugTo 0 & rezip

zipntugs i x = zipper x & fromWithin traverse & tugs rightward i

-- How fast is creating and destroying a zipper compared to
-- a regular traversal?
zipTraverseRezip x = zipper x & fromWithin traverse & rezip

-- is 'downward' any faster than the composition of traverse?
downwardAndRezip1 :: (String, String) -> (String, String)
downwardAndRezip1 xs =
  zipper xs & downward _1 & fromWithin traverse & focus .~ 'h' & rezip
downwardAndRezip2 :: (String, String) -> (String, String)
downwardAndRezip2 xs =
  zipper xs & fromWithin (_1.traverse) & focus .~ 'g' & rezip
