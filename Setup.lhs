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

import Distribution.Extra.Doctest ( generateBuildModule )

#else

#ifdef MIN_VERSION_Cabal
-- If the macro is defined, we have new cabal-install,
-- but for some reason we don't have cabal-doctest in package-db
--
-- Probably we are running cabal sdist, when otherwise using new-build
-- workflow
import Warning ()
#endif

generateBuildModule :: a -> b -> c -> d -> IO ()
generateBuildModule _ _ _ _ = return ()

#endif

haddockOutputDir :: Package p => HaddockFlags -> p -> FilePath
haddockOutputDir flags pkg = destDir where
  baseDir = case haddockDistPref flags of
    NoFlag -> "."
    Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule "doctests" flags pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  , postHaddock = \args flags pkg lbi -> do
     copyFiles normal (haddockOutputDir flags pkg) [("images","Hierarchy.png")]
     postHaddock simpleUserHooks args flags pkg lbi
  }

\end{code}
