\begin{code}
{-# LANGUAGE CPP #-}
-- If compiler doesn't give us cabal version, let's assume Cabal is old as well.
-- GHC-8.0.1 defines MIN_VERSION_ macros, and it's also the first GHC with
-- bundled Cabal >= 1.24
--
-- Luckily when setup-depends is specified, recent enough `cabal`
-- defines MIN_VERSION_ pragmas too.
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0
#endif
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
#if MIN_VERSION_Cabal(1,24,0)
import Distribution.Package ( UnitId (..),  ComponentId (..) )
#else
-- Unfortunately there aren't way to extract package-id from InstalledPackageId
-- across different Cabal versions (CPP macro isn't reliable).
-- So we have fallback to bare package name + version
import Distribution.Package ( PackageName(PackageName), InstalledPackageId, packageVersion )
import Data.Version ( showVersion )
#endif
import Distribution.Package ( PackageId, Package, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose, copyFiles )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), Flag(..), fromFlag, HaddockFlags(haddockDistPref))
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps), compiler, buildDir )
import Distribution.Simple.Compiler ( showCompilerId )
import Distribution.Text ( display )
import Distribution.Verbosity ( Verbosity, normal )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  , postHaddock = \args flags pkg lbi -> do
     copyFiles normal (haddockOutputDir flags pkg) [("images","Hierarchy.png")]
     postHaddock simpleUserHooks args flags pkg lbi
  }

haddockOutputDir :: Package p => HaddockFlags -> p -> FilePath
haddockOutputDir flags pkg = destDir where
  baseDir = case haddockDistPref flags of
    NoFlag -> "."
    Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  let bdir = buildDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , ""
        , "autogen_dir :: String"
        , "autogen_dir = " ++ show dir
        , ""
        , "build_dir :: String"
        , "build_dir = " ++ show bdir
        , ""
        , "deps :: [String]"
        , "deps = " ++ (show $ formatDeps (testDeps libcfg suitecfg))
        , ""
        , "compiler :: String"
        , "compiler = " ++ (show $ showCompilerId $ compiler lbi)
        , ""
        , "depsFlag :: String"
#if MIN_VERSION_Cabal(1,24,0)
        , "depsFlag = " ++ show "-package-id="
#else
        , "depsFlag = " ++ show "-package="
#endif
        ]


#if MIN_VERSION_Cabal(1,24,0)
testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(UnitId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

formatDeps :: [(UnitId, a)] -> [String]
formatDeps = map (formatone . fst)
  where
    formatone (SimpleUnitId (ComponentId i)) = i
#else
testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

formatDeps :: [(a, PackageId)] -> [String]
formatDeps = map (formatone . snd)
  where
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)
#endif

\end{code}
