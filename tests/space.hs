-- | Regression test for the lazy-'ByteString' space leak in 'lastOf' /
-- 'Rightmost' folds (see "Control.Lens.Internal.ByteString"; lens issue #233).
--
-- 'lastOf' over a lazy 'ByteString' must run in /constant/ space. A leaking
-- traversal retains roughly one thunk per chunk, so peak residency grows with
-- the chunk count. We fold a lazy 'ByteString' at two sizes (16x apart) and
-- assert that peak residency (from "GHC.Stats") does not grow: a streaming fold
-- stays flat, a leaking one jumps by megabytes. The input is built from a fixed
-- chunk size so the leak's magnitude is independent of bytestring's internal
-- chunking heuristics (which have varied across releases).
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Lens (bytes)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import           System.Environment (getArgs, getExecutablePath)
import           System.Exit (exitFailure, exitWith)
import           System.IO (hPutStrLn, stderr)
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

-- | A lazy 'ByteString' of @n@ fixed-size chunks. 'lastOf' must reach the end,
-- so the whole spine is traversed; the leak (if present) retains ~one thunk per
-- chunk, so its size scales with @n@.
chunked :: Int -> BL.ByteString
chunked n = BL.fromChunks (replicate n chunk)
  where chunk = B.replicate 4096 7

forceLast :: Int -> IO ()
forceLast n = do
  r <- evaluate (lastOf bytes (chunked n))
  unless (r == Just 7) (error ("space: unexpected lastOf result " ++ show r))

-- | Peak live bytes observed so far (max over all major GCs).
peakBytes :: IO Integer
peakBytes = performMajorGC >> fmap (toInteger . max_live_bytes) getRTSStats

runTest :: IO ()
runTest = do
  enabled <- getRTSStatsEnabled
  if not enabled
    -- The child is only ever reached via our own re-exec, which always passes
    -- -T, so a disabled-stats child means the harness itself is broken. Fail
    -- loudly rather than skip -- a silent pass would exercise nothing forever.
    then do
      hPutStrLn stderr "space: RTS stats not in effect after re-exec; harness broken."
      exitFailure
    else do
      let small =   8 * 1000   --   8k chunks (~32 MB)
          big   = 128 * 1000   -- 128k chunks (16x; ~512 MB)
          -- Streaming grows by a few KB; the leak grows by ~3 MB. 256 KB sits
          -- well inside that gap, with plenty of margin on both sides.
          cap   = 256 * 1000
      forceLast small
      p1 <- peakBytes
      forceLast big
      p2 <- peakBytes
      let growth = p2 - p1
      printf "lastOf bytes peak residency: %d B (8k chunks) -> %d B (128k chunks), growth %d B\n"
             p1 p2 growth
      when (growth > cap) $ do
        printf "FAIL: residency grew %d B over a 16x input increase; lastOf over a lazy ByteString is leaking (issue #233).\n"
               growth
        exitFailure
      putStrLn "OK: lastOf over a lazy ByteString streams in constant space."
