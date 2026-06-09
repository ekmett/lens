-- | Regression test for the lazy-'ByteString' space leak in 'lastOf' /
-- 'Rightmost' folds (see "Control.Lens.Internal.ByteString"; lens issue #233).
--
-- 'lastOf' over a lazy 'ByteString' must run in /constant/ space. A leaking
-- traversal retains roughly one thunk per chunk, so peak residency grows with
-- the input length. We fold a lazily-generated 'ByteString' at two sizes (16x
-- apart) and assert that peak residency (from "GHC.Stats") does not grow: a
-- streaming fold stays flat, a leaking one jumps by megabytes.
--
-- The peak is transient (it only exists during the fold), so it is sampled
-- reliably only if GCs are frequent: we need a small nursery (@-A256k@) and
-- @-T@ to enable "GHC.Stats". A program cannot choose its own RTS options after
-- startup, and baking a multi-word @-with-rtsopts@ through cabal is unreliable,
-- so on first entry we re-exec ourselves passing those options as @+RTS@
-- arguments (the executable is built with @-rtsopts@, so it accepts them). We
-- pass them as arguments rather than via the @GHCRTS@ environment variable so as
-- not to disturb any @GHCRTS@ the user may have set.
module Main (main) where

import           Control.Exception (evaluate)
import           Control.Lens (lastOf)
import           Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Lens (bytes)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import           System.Environment (getArgs, getExecutablePath)
import           System.Exit (exitFailure, exitWith)
import           System.Mem (performMajorGC)
import           System.Process (rawSystem)
import           Text.Printf (printf)

-- | Sentinel argument identifying the re-exec'd child. The RTS strips the
-- @+RTS ... -RTS@ block from the child's arguments, so this is what is left to
-- distinguish it from the initial invocation (and stops it re-exec'ing again).
childArg :: String
childArg = "--lens-space-test-child"

main :: IO ()
main = do
  args <- getArgs
  if childArg `elem` args
    then runTest
    else do
      exe <- getExecutablePath
      exitWith =<< rawSystem exe [childArg, "+RTS", "-T", "-A256k", "-RTS"]

-- | Drive 'lastOf' over the whole (lazily generated) ByteString. 'lastOf' must
-- reach the end, so the entire structure is traversed.
forceLast :: Int -> IO ()
forceLast n = do
  r <- evaluate (lastOf bytes (BL.replicate (fromIntegral n) 7))
  unless (r == Just 7) (error ("space: unexpected lastOf result " ++ show r))

-- | Peak live bytes observed so far (max over all major GCs).
peakBytes :: IO Integer
peakBytes = performMajorGC >> fmap (toInteger . max_live_bytes) getRTSStats

runTest :: IO ()
runTest = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "space: RTS stats unavailable (-T not in effect?); skipping."
    else do
      let small =  50 * 1000 * 1000   --  50 MB
          big   = 800 * 1000 * 1000   -- 800 MB (16x)
          cap   =   2 * 1000 * 1000   --   2 MB slack (a leak grows by ~4 MB here)
      forceLast small
      p1 <- peakBytes
      forceLast big
      p2 <- peakBytes
      let growth = p2 - p1
      printf "lastOf bytes peak residency: %d B (50MB) -> %d B (800MB), growth %d B\n"
             p1 p2 growth
      when (growth > cap) $ do
        printf "FAIL: residency grew %d B over a 16x input increase; lastOf over a lazy ByteString is leaking (issue #233).\n"
               growth
        exitFailure
      putStrLn "OK: lastOf over a lazy ByteString streams in constant space."
