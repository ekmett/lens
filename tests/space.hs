-- | Regression test for the space leak of 'lastOf' over a lazy 'ByteString'
-- (lens issue #233), of 'firstOf' over 'backwards', and of both over a
-- balanced fold (lens issue #1142). The fixes are in 'Rightmost' and
-- 'Leftmost' in "Control.Lens.Internal.Fold".
--
-- Over a balanced fold, such as 'Data.ByteString.Lens.bytes' on a strict
-- 'B.ByteString', 'firstOf' and 'lastOf' must keep memory for one path of the
-- tree, not for a whole level.
--
-- 'lastOf', and 'firstOf' over 'backwards', over a lazy 'ByteString' must run
-- in /constant/ space. A leaking fold retains memory per chunk (or per byte),
-- so peak residency grows with the input. We fold a lazy 'ByteString' at two
-- sizes (16x apart) and check that peak residency (from "GHC.Stats") stays
-- flat. 'growthOk' gives the limit. The input is built from a fixed chunk
-- size so the leak's magnitude is independent of bytestring's internal
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
import           Control.Lens (Fold, backwards, filtered, firstOf, folded, lastOf, to)
import           Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Lens (bytes)
import qualified Data.ByteString.Strict.Lens as Strict
import           Data.Word (Word8)
import           GHC.RTS.Flags (getGCFlags, minAllocAreaSize)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.Mem (performMajorGC)
import           Text.Printf (printf)

-- | A lazy 'ByteString' of @n@ chunks of @size@ bytes, all 7s. 'lastOf', and
-- 'firstOf' over 'backwards', must reach the end, so the whole spine is
-- traversed.
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

-- | A lazy 'ByteString' of @n@ chunks of @size@ bytes, each chunk a new
-- buffer, as from lazy IO. The last byte is @n `mod` 200@.
freshChunks :: Int -> Int -> BL.ByteString
freshChunks size n = BL.fromChunks [ B.replicate size (fromIntegral (i `mod` 200)) | i <- [1 .. n] ]

-- | Check that a fold over 'freshChunks' raises the peak residency by at most
-- 256 KB. Every chunk that the search keeps shows in the peak, so this also
-- catches a search that keeps many chunks but not more for a longer input.
freshOk :: String -> (BL.ByteString -> Maybe Word8) -> Int -> Int -> IO Bool
freshOk name fold size n = do
  p1 <- peakBytes
  r <- evaluate (fold (freshChunks size n))
  unless (r == Just (fromIntegral (n `mod` 200))) (error ("space: unexpected " ++ name ++ " result " ++ show r))
  p2 <- peakBytes
  let growth = p2 - p1
      ok = growth <= 256 * 1024
  printf "%s, %d new %d-byte chunks: peak residency %d B -> %d B, growth %d B: %s\n"
         name n size p1 p2 growth (if ok then "OK" else "FAIL")
  pure ok
-- NOINLINE: so GHC does not float the input out and keep it alive.
{-# NOINLINE freshOk #-}

-- | The folds under test. 'lastNone' and 'lastNested' give 'Rightmost' values
-- other than 'RLeaf'. A fold can leak on these when 'lastBytes' does not.
lastBytes, lastNone, lastNested :: BL.ByteString -> Maybe Word8
lastBytes = lastOf bytes
-- No byte is a target, so each byte gives 'RPure'.
lastNone = lastOf (bytes . filtered (/= 7))
-- Each byte gives an 'RNode', not an 'RLeaf'.
lastNested = lastOf (bytes . to (: []) . folded)

-- | 'backwards' makes the fold nest to the left, so these folds test
-- 'Leftmost'. See also 'lastBytes' and 'lastNone', which test 'Rightmost'.
firstBack, firstBackNone :: BL.ByteString -> Maybe Word8
firstBack = firstOf (backwards bytes)
-- No byte is a target, so each byte gives 'LPure'.
firstBackNone = firstOf (backwards (bytes . filtered (/= 7)))

-- | The numbers from 1 to @n@, folded as a balanced tree. The input is only
-- @n@, so all the memory is for the fold.
balanced :: Fold Int Int
balanced f n = go 1 n
  where
    go lo hi
      | lo >= hi  = f lo
      | otherwise = let mid = (lo + hi) `div` 2 in go lo mid *> go (mid + 1) hi

-- | Check that the peak residency of a fold over 'balanced' grows by at most
-- 256 KB from @small@ to @big@ elements. A search that keeps a whole level of
-- the tree grows by megabytes.
balancedOk :: String -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Int -> IO Bool
balancedOk name fold expected small big = do
  let run n = do
        r <- evaluate (fold n)
        unless (r == expected n) (error ("space: unexpected " ++ name ++ " result " ++ show r))
      cap = 256 * 1024
  run small
  p1 <- peakBytes
  run big
  p2 <- peakBytes
  let growth = p2 - p1
      ok = growth <= cap
  printf "%s: peak residency %d B (%d elements) -> %d B (%d elements), growth %d B: %s\n"
         name p1 small p2 big growth (if ok then "OK" else "FAIL")
  pure ok
{-# NOINLINE balancedOk #-}

-- | Check a fold over a strict 'B.ByteString' of @small@ and @big@ bytes, all
-- 7s. The input itself grows by @big - small@ bytes; the fold may add at most
-- 256 KB more.
strictOk :: String -> (B.ByteString -> Maybe Word8) -> Maybe Word8 -> Int -> Int -> IO Bool
strictOk name fold expected small big = do
  let run n = do
        r <- evaluate (fold (B.replicate n 7))
        unless (r == expected) (error ("space: unexpected " ++ name ++ " result " ++ show r))
      cap = toInteger (big - small) + 256 * 1024
  run small
  p1 <- peakBytes
  run big
  p2 <- peakBytes
  let growth = p2 - p1
      ok = growth <= cap
  printf "%s: peak residency %d B (%d bytes) -> %d B (%d bytes), growth %d B (input %d B): %s\n"
         name p1 small p2 big growth (big - small) (if ok then "OK" else "FAIL")
  pure ok
{-# NOINLINE strictOk #-}

main :: IO ()
main = do
  checkRtsOpts
  -- Balanced folds first: their peaks are the smallest.
  okA <- balancedOk "firstOf (balanced . filtered (< 0))" (firstOf (balanced . filtered (< 0))) (const Nothing) 16384 262144
  okB <- balancedOk "lastOf (balanced . filtered (< 0))" (lastOf (balanced . filtered (< 0))) (const Nothing) 16384 262144
  okC <- balancedOk "lastOf balanced" (lastOf balanced) Just 16384 262144
  -- A leak per byte shows on small inputs (128 KB -> 2 MB).
  ok1 <- growthOk "lastOf (bytes . filtered (/= 7))" lastNone Nothing 4096 32 512
  ok2 <- growthOk "lastOf (bytes . to (:[]) . folded)" lastNested (Just 7) 4096 32 512
  -- A leak per chunk needs many chunks. With no target, chunks of 256 B or
  -- less hide the leak, so this case uses 1 KB chunks. Its leak is smaller
  -- than the next case's leak, so it runs first.
  ok3 <- growthOk "lastOf (bytes . filtered (/= 7))" lastNone Nothing 1024 8000 128000
  ok4 <- growthOk "lastOf bytes" lastBytes (Just 7) 4096 8000 128000
  -- The mirror images of the two cases above, in the same order.
  ok5 <- growthOk "firstOf (backwards (bytes . filtered (/= 7)))" firstBackNone Nothing 1024 8000 128000
  ok6 <- growthOk "firstOf (backwards bytes)" firstBack (Just 7) 4096 8000 128000
  -- Every byte is a target, so the search goes on after 'Just'.
  ok8 <- freshOk "lastOf bytes" lastBytes 4096 2000
  ok9 <- freshOk "firstOf (backwards bytes)" firstBack 4096 2000
  -- The input of this case is 1 MB and then 4 MB, so it runs last.
  ok7 <- strictOk "firstOf (Strict.bytes . filtered (/= 7))" (firstOf (Strict.bytes . filtered (/= 7))) Nothing 1048576 4194304
  unless (and [okA, okB, okC, ok1, ok2, ok3, ok4, ok5, ok6, ok8, ok9, ok7]) $ do
    putStrLn "FAIL: a fold is leaking; see the cases marked FAIL above (issues #233 and #1142)."
    exitFailure
  putStrLn "OK: lastOf and firstOf run in constant space over a lazy ByteString, and in one path's space over a balanced fold."
