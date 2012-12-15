module Main where

import Test.DocTest
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = getSources >>= \sources -> doctest $
    "-isrc"
  : "-idist/build/autogen"
  : "-optP-include"
  : "-optPdist/build/autogen/cabal_macros.h"
  : "-hide-all-packages"
  : "-package=array"
  : "-package=base"
  : "-package=bytestring"
  : "-package=comonad"
  : "-package=comonad-transformers"
  : "-package=comonads-fd"
  : "-package=containers"
  : "-package=filepath"
  : "-package=ghc-prim"
  : "-package=hashable"
  : "-package=mtl"
  : "-package=parallel"
  : "-package=semigroups"
  : "-package=simple-reflect"
  : "-package=split"
  : "-package=template-haskell"
  : "-package=text"
  : "-package=transformers"
  : "-package=unordered-containers"
  : "-package=vector"
  : sources

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "src"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
