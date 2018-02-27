\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

-- haddock stuff
import Distribution.Package ( Package (..), packageName )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Setup (Flag (..), HaddockFlags, haddockDistPref)
import Distribution.Simple.Utils (copyFiles)
import Distribution.Verbosity (normal)
import Distribution.Text ( display )
import System.FilePath ( (</>) )

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest ( doctestsUserHooks )

#else

#ifdef MIN_VERSION_Cabal
-- If the macro is defined, we have new cabal-install,
-- but for some reason we don't have cabal-doctest in package-db
--
-- Probably we are running cabal sdist, when otherwise using new-build
-- workflow
import Warning ()
#endif

doctestsUserHooks :: String -> UserHooks
doctestsUserHooks _ = simpleUserHooks

#endif

haddockOutputDir :: Package p => HaddockFlags -> p -> FilePath
haddockOutputDir flags pkg = destDir where
  baseDir = case haddockDistPref flags of
    NoFlag -> "."
    Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

main :: IO ()
main = defaultMainWithHooks duh
  { postHaddock = \args flags pkg lbi -> do
     copyFiles normal (haddockOutputDir flags pkg) [("images","Hierarchy.png")]
     postHaddock duh args flags pkg lbi
  }
  where
    duh = doctestsUserHooks "doctests"

\end{code}
