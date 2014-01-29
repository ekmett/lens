-----------------------------------------------------------------------------
-- |
-- Module      :  Main (hlint)
-- Copyright   :  (C) 2013-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module runs HLint on the lens source tree.
-----------------------------------------------------------------------------
module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    hints <- hlint $ ["src", "--cpp-define=HLINT", "--cpp-ansi"] ++ args
    unless (null hints) exitFailure
