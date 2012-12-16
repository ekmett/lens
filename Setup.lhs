#!/usr/bin/runhaskell
\begin{code}
module Main (main) where

import Data.List ( nub )
import Data.Version ( showVersion )
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(libraryConfig, testSuiteConfigs), ComponentLocalBuildInfo(componentPackageDeps) )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule "doctests" (fromFlag (buildVerbosity flags)) lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  -- , haddockHook = \pkg lbi hooks flags -> do
  --   generateBuildModule (fromFlag (haddockVerbosity flags)) lbi
  --   haddockHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: String -> Verbosity -> LocalBuildInfo -> IO ()
generateBuildModule testSuite verbosity lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  rewriteFile (dir </> "Build_" ++ testSuite ++ ".hs") $ unlines
    [ "module Build_" ++ testSuite ++ " where"
    , "deps :: [String]"
    , "deps = " ++ (show $ formatdeps (testDeps testSuite lbi))
    ]
  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

testDeps :: String -> LocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps testSuite lbi = nub $
     maybe [] componentPackageDeps (lookup testSuite (testSuiteConfigs lbi))
  ++ maybe [] componentPackageDeps (libraryConfig lbi)

\end{code}
