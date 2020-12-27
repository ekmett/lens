-----------------------------------------------------------------------------
-- |
-- Module      :  Main (doctests)
-- Copyright   :  (C) 2012-14 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module exists to add dependencies
-----------------------------------------------------------------------------
module Main where

main :: IO ()
main = do
    putStrLn "This test-suite exists only to add dependencies"
    putStrLn "To run doctests: "
    putStrLn "    cabal build all --enable-tests"
    putStrLn "    cabal-docspec"
