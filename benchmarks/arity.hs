{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- Benchmark for issue #183: combinators used *under-applied*.
--
-- The setter wrappers are NOINLINE so the partially-applied form (`set _1`)
-- survives as a real binding. With `set` at arity 2 it cannot inline into the
-- wrapper and each application pays for the `Identity` newtype path; with `set`
-- at arity 1 the wrapper saturates it, `set` inlines, and worker/wrapper fuses
-- it to a tight unboxed-tuple worker. See NOTE: [Inlining and arity] in Control.Lens.Fold.
module Main (main) where

import Control.Lens
import Criterion.Main

-- `set _1` held under-applied at the definition site.
setFst :: Int -> (Int, Int) -> (Int, Int)
setFst = set _1
{-# NOINLINE setFst #-}

set'Snd :: Int -> (Int, Int) -> (Int, Int)
set'Snd = set' _2
{-# NOINLINE set'Snd #-}

-- `partsOf traverse` held under-applied (arity 3 -> 1).
reverseParts :: [Int] -> [Int]
reverseParts = over (partsOf traverse) reverse
{-# NOINLINE reverseParts #-}

-- scanl1Of drives mapAccumLOf under-applied (arity 4 -> 3).
scanSum :: [Int] -> [Int]
scanSum = scanl1Of traverse (+)
{-# NOINLINE scanSum #-}

-- Hammer the under-applied setter in a tight loop.
loopSet :: (Int -> (Int, Int) -> (Int, Int)) -> Int -> Int
loopSet f = \n ->
  let go !i !p@(a, b)
        | i >= n    = a + b
        | otherwise = go (i + 1) (f i p)
  in go 0 (0, 0)
{-# INLINE loopSet #-}

xs :: [Int]
xs = [1 .. 100]
{-# NOINLINE xs #-}

main :: IO ()
main = defaultMain
  [ bgroup "set-underapplied"
    [ bench "set _1"  $ whnf (loopSet setFst)  1000000
    , bench "set' _2" $ whnf (loopSet set'Snd) 1000000
    ]
  , bgroup "traversal-underapplied"
    [ bench "partsOf traverse" $ nf (\v -> sum (concatMap reverseParts (replicate 1000 v))) xs
    , bench "scanl1Of traverse" $ nf (\v -> sum (concatMap scanSum     (replicate 1000 v))) xs
    ]
  ]
