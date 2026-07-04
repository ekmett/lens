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
-- reliably only if GCs are frequent: the cabal stanza bakes in a small nursery
-- (@-A256k@) plus @-T@ to enable "GHC.Stats" via @-with-rtsopts@. Because the
-- whole test silently measures nothing if those options go missing, 'main'
-- first verifies through "GHC.RTS.Flags" that they are actually in effect and
-- fails loudly otherwise.
module Main (main) where

import           Control.Exception (evaluate)
import           Control.Lens (lastOf)
import           Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Lens (bytes)
import           GHC.RTS.Flags (getGCFlags, minAllocAreaSize)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.Mem (performMajorGC)
import           Text.Printf (printf)

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

-- | Fail loudly unless the RTS options this test depends on are in effect:
-- @-T@ (so "GHC.Stats" works at all) and a small allocation area (so the
-- transient peak is sampled often enough to be seen). Without this check, a
-- build that lost @-with-rtsopts@ would turn the suite into a permanent silent
-- pass that exercises nothing. Note that GHC does not relink when only
-- link-time flags such as @-with-rtsopts@ change, so an incremental build can
-- leave a binary with stale baked RTS options; this check catches that too.
checkRtsOpts :: IO ()
checkRtsOpts = do
  statsEnabled <- getRTSStatsEnabled
  gcFlags <- getGCFlags
  -- \-A256k = 64 four-KiB blocks; the default is 1024. Anything in that
  -- ballpark samples frequently enough.
  let nurseryOk = minAllocAreaSize gcFlags <= 128
  unless (statsEnabled && nurseryOk) $ do
    hPutStrLn stderr "space: -T/-A256k not in effect; was -with-rtsopts dropped from the cabal\
                     \ stanza, or did an incremental build skip the relink? (Deleting this\
                     \ test's build directory forces a correct relink.)"
    exitFailure

main :: IO ()
main = do
  checkRtsOpts
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
