-- | Regression test for the space leak of 'lastOf' over a lazy 'ByteString'
-- (lens issue #233). The fix is in 'Rightmost' in "Control.Lens.Internal.Fold".
--
-- 'lastOf' over a lazy 'ByteString' must run in /constant/ space. A leaking
-- fold retains memory per chunk (or per byte), so peak residency grows with
-- the input. We fold a lazy 'ByteString' at two sizes (16x apart) and
-- check that peak residency (from "GHC.Stats") stays flat. 'growthOk' gives
-- the limit. The input is built from a fixed chunk size so the leak's
-- magnitude is independent of bytestring's internal chunking heuristics
-- (which have varied across releases).
--
-- The peak is transient (it only exists during the fold), so it is sampled
-- reliably only if GCs are frequent: the cabal stanza bakes in a small nursery
-- (@-A256k@) plus @-T@ to enable "GHC.Stats" via @-with-rtsopts@. Because the
-- whole test silently measures nothing if those options go missing, 'main'
-- first verifies through "GHC.RTS.Flags" that they are actually in effect and
-- fails loudly otherwise.
module Main (main) where

import           Control.Exception (evaluate)
import           Control.Lens (filtered, folded, lastOf, to)
import           Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Lens (bytes)
import           Data.Word (Word8)
import           GHC.RTS.Flags (getGCFlags, minAllocAreaSize)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.Mem (performMajorGC)
import           Text.Printf (printf)

-- | A lazy 'ByteString' of @n@ chunks of @size@ bytes, all 7s. 'lastOf' must
-- reach the end, so the whole spine is traversed.
chunked :: Int -> Int -> BL.ByteString
chunked size n = BL.fromChunks (replicate n chunk)
  where chunk = B.replicate size 7

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

-- | Check that the peak residency of one fold grows by at most 256 KB from
-- @small@ to @big@ chunks of @size@ bytes. Streaming grows by a few KB; a
-- leak grows by hundreds of KB or more. 'peakBytes' is a running maximum, so
-- a case can only show growth above the peaks of the cases before it: run
-- the cases with the smaller leaks first.
growthOk :: String -> (BL.ByteString -> Maybe Word8) -> Maybe Word8 -> Int -> Int -> Int -> IO Bool
growthOk name fold expected size small big = do
  let run n = do
        r <- evaluate (fold (chunked size n))
        unless (r == expected) (error ("space: unexpected " ++ name ++ " result " ++ show r))
      cap = 256 * 1024
  run small
  p1 <- peakBytes
  run big
  p2 <- peakBytes
  let growth = p2 - p1
      ok = growth <= cap
  printf "%s, %d-byte chunks: peak residency %d B (%d chunks) -> %d B (%d chunks), growth %d B: %s\n"
         name size p1 small p2 big growth (if ok then "OK" else "FAIL")
  pure ok
-- NOINLINE: so GHC does not float the input out of 'run' and keep it alive.
{-# NOINLINE growthOk #-}

-- | The folds under test. 'lastNone' and 'lastNested' give 'Rightmost' values
-- other than 'RLeaf'. A fold can leak on these when 'lastBytes' does not.
lastBytes, lastNone, lastNested :: BL.ByteString -> Maybe Word8
lastBytes = lastOf bytes
-- No byte is a target, so each byte gives 'RPure'.
lastNone = lastOf (bytes . filtered (/= 7))
-- Each byte gives an 'RStep', not an 'RLeaf'.
lastNested = lastOf (bytes . to (: []) . folded)

main :: IO ()
main = do
  checkRtsOpts
  -- A leak per byte shows on small inputs (128 KB -> 2 MB).
  ok1 <- growthOk "lastOf (bytes . filtered (/= 7))" lastNone Nothing 4096 32 512
  ok2 <- growthOk "lastOf (bytes . to (:[]) . folded)" lastNested (Just 7) 4096 32 512
  -- A leak per chunk needs many chunks. With no target, chunks of 256 B or
  -- less hide the leak, so this case uses 1 KB chunks. Its leak is smaller
  -- than the next case's leak, so it runs first.
  ok3 <- growthOk "lastOf (bytes . filtered (/= 7))" lastNone Nothing 1024 8000 128000
  ok4 <- growthOk "lastOf bytes" lastBytes (Just 7) 4096 8000 128000
  unless (ok1 && ok2 && ok3 && ok4) $ do
    putStrLn "FAIL: lastOf over a lazy ByteString is leaking; see the cases marked FAIL above (issue #233)."
    exitFailure
  putStrLn "OK: lastOf over a lazy ByteString streams in constant space."
