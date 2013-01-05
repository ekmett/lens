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
      ]
  ]

-- The performance of rezipping a zipper.

tugAndRezip1, tugAndRezip2, tugAndRezip3 :: String -> String
tugAndRezip1 xs = zipntugs 25 xs & focus .~ 'a' & rezip
tugAndRezip2 xs = zipntugs 25 xs & focus .~ 'b' & farthest leftward & rezip
tugAndRezip3 xs = zipntugs 25 xs & focus .~ 'c' & leftmost & rezip

zipntugs i x = zipper x & fromWithin traverse & tugs rightward i
{-# INLINE zipntugs #-}
